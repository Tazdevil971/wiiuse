/*
 *	wiiuse
 *
 *	Written By:
 *		Michael Laforest	< para >
 *		Email: < thepara (--AT--) g m a i l [--DOT--] com >
 *
 *	Copyright 2006-2007
 *
 *	This file is part of wiiuse.
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *	$Header$
 *
 */

/**
 *	@file
 *	@brief Handles device I/O for Windows.
 */

#include <time.h>

#include "events.h"
#include "io.h"
#include "os.h"

#ifdef WIIUSE_WIN32
#include <stdlib.h>

#include <hidsdi.h>
#include <setupapi.h>
#include <bluetoothapis.h>

#include <initguid.h> // Devpkey.h
#include <Devpkey.h>  // DEVPKEY_Device_Parent

#include <wchar.h> // wcsstr, wcscmp, ...

#ifdef __MINGW32__
/* this prototype is missing from the mingw headers so we must add it
        or suffer linker errors. */
#ifdef __cplusplus
extern "C" {
#endif
WINHIDSDI BOOL WINAPI HidD_SetOutputReport(HANDLE, PVOID, ULONG);
#ifdef __cplusplus
}
#endif
#endif

static int clock_gettime(int X, struct timeval *tv);

int wiiuse_os_find(struct wiimote_t **wm, int max_wiimotes, int timeout)
{
    BLUETOOTH_FIND_RADIO_PARAMS radio_params;
    BLUETOOTH_DEVICE_SEARCH_PARAMS device_params;
    HBLUETOOTH_RADIO_FIND radio_enumerator;
    HBLUETOOTH_DEVICE_FIND device_enumerator;
    BLUETOOTH_DEVICE_INFO btdi;
    HANDLE radio_handle;
    HANDLE process_handle;
    HANDLE clone_radio_handle;
    WIIUSE_WIIMOTE_TYPE wiimote_type;
    const char *wiimote_type_str;
    char wiimote_addr_str[18]; // 12 digits + 5 colons + terminator

    int i;
    int wiimotes_count = 0;

    process_handle = GetCurrentProcess();

    device_params.dwSize               = sizeof(BLUETOOTH_DEVICE_SEARCH_PARAMS);
    device_params.fReturnAuthenticated = TRUE;
    device_params.fReturnConnected     = FALSE;
    device_params.fReturnUnknown       = TRUE;
    device_params.cTimeoutMultiplier   = (UCHAR)timeout;

    radio_params.dwSize = sizeof(BLUETOOTH_FIND_RADIO_PARAMS);

    btdi.dwSize = sizeof(BLUETOOTH_DEVICE_INFO);

    // We have to do two iterations because (for some reason) we cannot connect
    // to a remembered bluetooth device, so we first need to forget all of them.
    // Also remembered devices may not be visible, but only rememebered
    for (i = 0; i < 2; i++)
    {
        if (i == 0)
        { // The first time we do not need a full inquiry
            device_params.fReturnRemembered = TRUE;
            device_params.fIssueInquiry     = FALSE;
        } else
        {
            device_params.fReturnRemembered = FALSE;
            device_params.fIssueInquiry     = TRUE;
        }

        radio_enumerator = BluetoothFindFirstRadio(&radio_params, &radio_handle);
        if (radio_enumerator == NULL)
        {
            WIIUSE_ERROR("Could not detect a Bluetooth adapter!");
            return 0;
        }

        do
        {
            device_enumerator = BluetoothFindFirstDevice(&device_params, &btdi);
            if (device_enumerator == NULL)
                continue;

            do
            {
                // Bug #1: windows doesn't properly filter the devices, so we need check for ourself
                // Bug #2: this is not a correct boolean, in fact this is 32 for true and 0 for false
                if (btdi.fConnected)
                    continue;

                if (wcscmp(btdi.szName, WM_LONG_NAME) == 0)
                {
                    wiimote_type     = WIIUSE_WIIMOTE_REGULAR;
                    wiimote_type_str = " (regular wiimote)";

                } else if (wcscmp(btdi.szName, WM_LONG_NAME_TR) == 0)
                {
                    wiimote_type     = WIIUSE_WIIMOTE_MOTION_PLUS_INSIDE;
                    wiimote_type_str = " (motion plus inside)";

                } else
                {
                    continue;
                }

                // This is a wiimote

                if (i == 0)
                {
                    BluetoothRemoveDevice(&btdi.Address);
                } else
                {
                    snprintf(wiimote_addr_str, sizeof(wiimote_addr_str), "%02X:%02X:%02X:%02X:%02X:%02X",
                             btdi.Address.rgBytes[5], btdi.Address.rgBytes[4], btdi.Address.rgBytes[3],
                             btdi.Address.rgBytes[2], btdi.Address.rgBytes[1], btdi.Address.rgBytes[0]);

                    // Duplicate the radio handle for each device, so that we can cleanup better, and each
                    // device gets its instance
                    DuplicateHandle(process_handle, radio_handle, process_handle, &clone_radio_handle, 0,
                                    TRUE, DUPLICATE_SAME_ACCESS);

                    WIIMOTE_ENABLE_STATE(wm[wiimotes_count], WIIMOTE_STATE_DEV_FOUND);
                    wm[wiimotes_count]->btdi         = btdi;
                    wm[wiimotes_count]->radio_handle = clone_radio_handle;
                    wm[wiimotes_count]->type         = wiimote_type;

                    WIIUSE_INFO("Found wiimote (type: %s) (%s) [id %i].", wiimote_type_str, wiimote_addr_str,
                                wm[wiimotes_count]->unid);

                    wiimotes_count++;
                }

            } while (BluetoothFindNextDevice(device_enumerator, &btdi) != FALSE
                     && wiimotes_count < max_wiimotes);
            BluetoothFindDeviceClose(device_enumerator);

        } while (BluetoothFindNextRadio(radio_enumerator, &radio_handle) != FALSE
                 && wiimotes_count < max_wiimotes);
        BluetoothFindRadioClose(radio_enumerator);
        CloseHandle(radio_handle);
    }

    return wiimotes_count;
}

int wiiuse_os_connect(struct wiimote_t **wm, int wiimotes)
{
    GUID hid_guid;
    HDEVINFO devinfos;
    SP_DEVICE_INTERFACE_DATA device_data;
    SP_DEVINFO_DATA devinfo_data;
    SP_DEVICE_INTERFACE_DETAIL_DATA_W *device_detail_data = NULL;
    HIDD_ATTRIBUTES hid_attributes;

    HANDLE device_handle = INVALID_HANDLE_VALUE;
    WCHAR parent[256];
    WCHAR address_str[13]; // 12 digits + terminator
    DEVPROPTYPE parent_type;
    DWORD len;

    int i, ii;
    int wiimotes_count = 0;

    device_data.cbSize  = sizeof(SP_DEVICE_INTERFACE_DATA);
    devinfo_data.cbSize = sizeof(SP_DEVINFO_DATA);
    hid_attributes.Size = sizeof(HIDD_ATTRIBUTES);

    // Connect all of the devices
    for (i = 0; i < wiimotes; i++)
        if (WIIMOTE_IS_SET(wm[i], WIIMOTE_STATE_DEV_FOUND))
            BluetoothSetServiceState(wm[i]->radio_handle, &wm[i]->btdi,
                                     &HumanInterfaceDeviceServiceClass_UUID, BLUETOOTH_SERVICE_ENABLE);

    HidD_GetHidGuid(&hid_guid);

    devinfos = SetupDiGetClassDevsW(&hid_guid, NULL, NULL, DIGCF_PRESENT | DIGCF_DEVICEINTERFACE);
    for (i = 0;
         SetupDiEnumDeviceInterfaces(devinfos, NULL, &hid_guid, i, &device_data) && wiimotes_count < wiimotes;
         i++)
    {
        if (device_detail_data)
        {
            free(device_detail_data);
            device_detail_data = NULL;
        }

        if (device_handle != INVALID_HANDLE_VALUE)
        {
            CloseHandle(device_handle);
            device_handle = INVALID_HANDLE_VALUE;
        }

        SetupDiGetDeviceInterfaceDetailW(devinfos, &device_data, NULL, 0, &len, NULL);

        device_detail_data         = (SP_DEVICE_INTERFACE_DETAIL_DATA_W *)malloc(len);
        device_detail_data->cbSize = sizeof(SP_DEVICE_INTERFACE_DETAIL_DATA_W);

        if (SetupDiGetDeviceInterfaceDetailW(devinfos, &device_data, device_detail_data, len, NULL,
                                             &devinfo_data)
            == FALSE)
            continue;

        device_handle = CreateFileW(device_detail_data->DevicePath, GENERIC_READ | GENERIC_WRITE,
                                    FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING,
                                    FILE_FLAG_OVERLAPPED, NULL);
        if (device_handle == INVALID_HANDLE_VALUE)
            continue;

        HidD_GetAttributes(device_handle, &hid_attributes);
        if (hid_attributes.VendorID != WM_VENDOR_ID
            || (hid_attributes.ProductID != WM_PRODUCT_ID && hid_attributes.ProductID != WM_PRODUCT_ID_TR))
            continue;

        // This is a wiimote

        // Get the device parent string (wich should contain the MAC somewhere)
        if (SetupDiGetDevicePropertyW(devinfos, &devinfo_data, &DEVPKEY_Device_Parent, &parent_type,
                                      (PBYTE)parent, sizeof(parent), NULL, 0)
            != TRUE)
            continue;

        for (ii = 0; ii < wiimotes; ii++)
        {
            if (WIIMOTE_IS_SET(wm[ii], WIIMOTE_STATE_DEV_FOUND))
            {
                // Generate the MAC string (windows stores it in reverse order)
                wsprintfW(address_str, L"%02X%02X%02X%02X%02X%02X", wm[ii]->btdi.Address.rgBytes[5],
                          wm[ii]->btdi.Address.rgBytes[4], wm[ii]->btdi.Address.rgBytes[3],
                          wm[ii]->btdi.Address.rgBytes[2], wm[ii]->btdi.Address.rgBytes[1],
                          wm[ii]->btdi.Address.rgBytes[0]);

                if (wcsstr(parent, address_str) != NULL)
                {
                    // The MAC matches

                    wm[ii]->dev_handle             = device_handle;
                    wm[ii]->hid_overlap.hEvent     = CreateEvent(NULL, 1, 1, "");
                    wm[ii]->hid_overlap.Offset     = 0;
                    wm[ii]->hid_overlap.OffsetHigh = 0;

                    device_handle = INVALID_HANDLE_VALUE;

                    WIIMOTE_ENABLE_STATE(wm[ii], WIIMOTE_STATE_CONNECTED);

                    /* try to set the output report to see if the device is actually connected */
                    if (!wiiuse_set_report_type(wm[ii]))
                    {
                        WIIMOTE_DISABLE_STATE(wm[ii], WIIMOTE_STATE_CONNECTED);
                        continue;
                    }

                    WIIUSE_INFO("Connected to wiimote [id %i].", wm[ii]->unid);

                    /* do the handshake */
                    wiiuse_handshake(wm[ii], NULL, 0);

                    wiimotes_count++;
                }
            }
        }
    }

    // If we haven't found the corresponding handle then disconnect the wiimote
    for (i = 0; i < wiimotes; i++)
        if (WIIMOTE_IS_SET(wm[i], WIIMOTE_STATE_DEV_FOUND) && !WIIMOTE_IS_CONNECTED(wm[i]))
            BluetoothSetServiceState(wm[i]->radio_handle, &wm[i]->btdi,
                                     &HumanInterfaceDeviceServiceClass_UUID, BLUETOOTH_SERVICE_DISABLE);

    if (device_detail_data)
        free(device_detail_data);

    if (device_handle != INVALID_HANDLE_VALUE)
        CloseHandle(device_handle);

    SetupDiDestroyDeviceInfoList(devinfos);

    return wiimotes_count;
}

void wiiuse_os_disconnect(struct wiimote_t *wm)
{
    if (!wm || WIIMOTE_IS_CONNECTED(wm))
        return;

    // Shut down the bluetooth connection
    BluetoothSetServiceState(wm->radio_handle, &wm->btdi, &HumanInterfaceDeviceServiceClass_UUID,
                             BLUETOOTH_SERVICE_DISABLE);

    CloseHandle(wm->radio_handle);
    wm->dev_handle = INVALID_HANDLE_VALUE;

    CloseHandle(wm->dev_handle);
    wm->dev_handle = INVALID_HANDLE_VALUE;

    ResetEvent(&wm->hid_overlap);

    wm->event = WIIUSE_NONE;

    WIIMOTE_DISABLE_STATE(wm, WIIMOTE_STATE_CONNECTED);
    WIIMOTE_DISABLE_STATE(wm, WIIMOTE_STATE_HANDSHAKE);
}

int wiiuse_os_poll(struct wiimote_t **wm, int wiimotes)
{
    int i;
    byte read_buffer[MAX_PAYLOAD];
    int evnt = 0;

    if (!wm)
    {
        return 0;
    }

    for (i = 0; i < wiimotes; ++i)
    {
        wm[i]->event = WIIUSE_NONE;

        /* clear out the buffer */
        memset(read_buffer, 0, sizeof(read_buffer));
        /* read */
        if (wiiuse_os_read(wm[i], read_buffer, sizeof(read_buffer)))
        {
            /* propagate the event */
            propagate_event(wm[i], read_buffer[0], read_buffer + 1);
            evnt += (wm[i]->event != WIIUSE_NONE);
        } else
        {
            /* send out any waiting writes */
            wiiuse_send_next_pending_write_request(wm[i]);
            idle_cycle(wm[i]);
        }
    }

    return evnt;
}

int wiiuse_os_read(struct wiimote_t *wm, byte *buf, int len)
{
    DWORD b, r;

    if (!wm || !WIIMOTE_IS_CONNECTED(wm))
    {
        return 0;
    }

    if (!ReadFile(wm->dev_handle, buf, len, &b, &wm->hid_overlap))
    {
        /* partial read */
        b = GetLastError();

        if ((b == ERROR_HANDLE_EOF) || (b == ERROR_DEVICE_NOT_CONNECTED))
        {
            /* remote disconnect */
            wiiuse_disconnected(wm);
            return 0;
        }

        r = WaitForSingleObject(wm->hid_overlap.hEvent, wm->timeout);
        if (r == WAIT_TIMEOUT)
        {
            /* timeout - cancel and continue */

            /*
                        if (*buf) {
                                WIIUSE_WARNING("Packet ignored.  This may indicate a problem (timeout is %i
               ms).", wm->timeout);
                        }
            */

            CancelIo(wm->dev_handle);
            ResetEvent(wm->hid_overlap.hEvent);
            return 0;
        } else if (r == WAIT_FAILED)
        {
            WIIUSE_WARNING("A wait error occurred on reading from wiimote %i.", wm->unid);
            return 0;
        }

        if (!GetOverlappedResult(wm->dev_handle, &wm->hid_overlap, &b, 0))
        {
            return 0;
        }

/* log the received data */
#ifdef WITH_WIIUSE_DEBUG
        {
            DWORD i;
            printf("[DEBUG] (id %i) RECV: (%.2x) ", wm->unid, buf[0]);
            for (i = 1; i < b; i++)
            {
                printf("%.2x ", buf[i]);
            }
            printf("\n");
        }
#endif
    }

    ResetEvent(wm->hid_overlap.hEvent);
    return 1;
}

int wiiuse_os_write(struct wiimote_t *wm, byte report_type, byte *buf, int len)
{
    DWORD bytes;
    int i;
    byte write_buffer[MAX_PAYLOAD];

    if (!wm || !WIIMOTE_IS_CONNECTED(wm))
    {
        return 0;
    }

    write_buffer[0] = report_type;
    memcpy(write_buffer + 1, buf, len);

    switch (wm->stack)
    {
    case WIIUSE_STACK_UNKNOWN:
    {
        /* try to auto-detect the stack type */
        if (i = WriteFile(wm->dev_handle, write_buffer, 22, &bytes, &wm->hid_overlap))
        {
            /* bluesoleil will always return 1 here, even if it's not connected */
            wm->stack = WIIUSE_STACK_BLUESOLEIL;
            return i;
        }

        if (i = HidD_SetOutputReport(wm->dev_handle, write_buffer, len + 1))
        {
            wm->stack = WIIUSE_STACK_MS;
            return i;
        }

        WIIUSE_ERROR("Unable to determine bluetooth stack type.");
        return 0;
    }

    case WIIUSE_STACK_MS:
        return HidD_SetOutputReport(wm->dev_handle, write_buffer, len + 1);

    case WIIUSE_STACK_BLUESOLEIL:
        return WriteFile(wm->dev_handle, write_buffer, 22, &bytes, &wm->hid_overlap);
    }

    return 0;
}

void wiiuse_init_platform_fields(struct wiimote_t *wm)
{
    wm->dev_handle     = INVALID_HANDLE_VALUE;
    wm->radio_handle   = INVALID_HANDLE_VALUE;
    wm->stack          = WIIUSE_STACK_UNKNOWN;
    wm->normal_timeout = WIIMOTE_DEFAULT_TIMEOUT;
    wm->exp_timeout    = WIIMOTE_EXP_TIMEOUT;
    wm->timeout        = wm->normal_timeout;
}

void wiiuse_cleanup_platform_fields(struct wiimote_t *wm)
{
    wm->dev_handle   = INVALID_HANDLE_VALUE;
    wm->radio_handle = INVALID_HANDLE_VALUE;
}

unsigned long wiiuse_os_ticks()
{
    unsigned long ms;
    struct timeval tp;

    clock_gettime(0, &tp);
    ms = (unsigned long)(1000 * tp.tv_sec + tp.tv_usec / 1e3);
    return ms;
}

/* code taken from http://stackoverflow.com/questions/5404277/porting-clock-gettime-to-windows/5404467#5404467
 */
static LARGE_INTEGER getFILETIMEoffset()
{
    SYSTEMTIME s;
    FILETIME f;
    LARGE_INTEGER t;

    s.wYear         = 1970;
    s.wMonth        = 1;
    s.wDay          = 1;
    s.wHour         = 0;
    s.wMinute       = 0;
    s.wSecond       = 0;
    s.wMilliseconds = 0;
    SystemTimeToFileTime(&s, &f);
    t.QuadPart = f.dwHighDateTime;
    t.QuadPart <<= 32;
    t.QuadPart |= f.dwLowDateTime;
    return (t);
}

static int clock_gettime(int X, struct timeval *tv)
{
    LARGE_INTEGER t;
    FILETIME f;
    double microseconds;
    static LARGE_INTEGER offset;
    static double frequencyToMicroseconds;
    static int initialized            = 0;
    static BOOL usePerformanceCounter = 0;

    if (!initialized)
    {
        LARGE_INTEGER performanceFrequency;
        initialized           = 1;
        usePerformanceCounter = QueryPerformanceFrequency(&performanceFrequency);
        if (usePerformanceCounter)
        {
            QueryPerformanceCounter(&offset);
            frequencyToMicroseconds = (double)performanceFrequency.QuadPart / 1000000.;
        } else
        {
            offset                  = getFILETIMEoffset();
            frequencyToMicroseconds = 10.;
        }
    }
    if (usePerformanceCounter)
        QueryPerformanceCounter(&t);
    else
    {
        GetSystemTimeAsFileTime(&f);
        t.QuadPart = f.dwHighDateTime;
        t.QuadPart <<= 32;
        t.QuadPart |= f.dwLowDateTime;
    }

    t.QuadPart -= offset.QuadPart;
    microseconds = (double)t.QuadPart / frequencyToMicroseconds;
    t.QuadPart   = (LONGLONG)microseconds;
    tv->tv_sec   = (long)t.QuadPart / 1000000;
    tv->tv_usec  = t.QuadPart % 1000000;
    return (0);
}
#endif /* ifdef WIIUSE_WIN32 */
