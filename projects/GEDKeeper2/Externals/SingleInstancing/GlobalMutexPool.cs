#if __MonoCS__

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Threading;

using GKCommon;

namespace Externals.SingleInstancing
{
    /// <summary>
    /// Low performance, system-wide mutex objects pool.
    /// </summary>
    public static class GlobalMutexPool
    {
        private static List<KeyValuePair<string, Mutex>> m_vMutexesWin = new List<KeyValuePair<string, Mutex>>();
        private static List<KeyValuePair<string, string>> m_vMutexesUnix = new List<KeyValuePair<string, string>>();

        private static int m_iLastRefresh = 0;

        private const double GmpMutexValidSecs = 190.0;
        private const int GmpMutexRefreshMs = 60 * 1000;

        public static bool CreateMutex(string strName, bool bInitiallyOwned)
        {
            return !SysInfo.IsUnix() ? CreateMutexWin(strName, bInitiallyOwned) : CreateMutexUnix(strName, bInitiallyOwned);
        }

        private static bool CreateMutexWin(string strName, bool bInitiallyOwned)
        {
            try
            {
                bool bCreatedNew;
                Mutex m = new Mutex(bInitiallyOwned, strName, out bCreatedNew);

                if(bCreatedNew)
                {
                    m_vMutexesWin.Add(new KeyValuePair<string, Mutex>(strName, m));
                    return true;
                }
            }
            catch(Exception) { }

            return false;
        }

        private static bool CreateMutexUnix(string strName, bool bInitiallyOwned)
        {
            string strPath = GetMutexPath(strName);
            try
            {
                if(File.Exists(strPath))
                {
                    byte[] pbEnc = File.ReadAllBytes(strPath);
                    byte[] pb = pbEnc;
                    if(pb.Length == 12)
                    {
                        long lTime = BitConverter.ToInt64(pb, 0);
                        DateTime dt = DateTime.FromBinary(lTime);

                        if((DateTime.UtcNow - dt).TotalSeconds < GmpMutexValidSecs)
                        {
                            int pid = BitConverter.ToInt32(pb, 8);
                            try
                            {
                                Process.GetProcessById(pid); // Throws if process is not running
                                return false; // Actively owned by other process
                            }
                            catch(Exception) { }
                        }

                        // Release the old mutex since process is not running
                        ReleaseMutexUnix(strName);
                    }
                    else { Debug.Assert(false); }
                }
            }
            catch(Exception) { Debug.Assert(false); }

            try {
                WriteMutexFilePriv(strPath);
            } catch(Exception) {
                Debug.Assert(false);
            }

            m_vMutexesUnix.Add(new KeyValuePair<string, string>(strName, strPath));
            return true;
        }

        private static void WriteMutexFilePriv(string strPath)
        {
            byte[] pb = new byte[12];
            BitConverter.GetBytes(DateTime.UtcNow.ToBinary()).CopyTo(pb, 0);
            BitConverter.GetBytes(Process.GetCurrentProcess().Id).CopyTo(pb, 8);
            File.WriteAllBytes(strPath, pb);
        }

        public static bool ReleaseMutex(string strName)
        {
            return !SysInfo.IsUnix() ? ReleaseMutexWin(strName) : ReleaseMutexUnix(strName);
        }

        private static bool ReleaseMutexWin(string strName)
        {
            for (int i = 0; i < m_vMutexesWin.Count; ++i)
            {
                if (m_vMutexesWin[i].Key.Equals(strName, StringComparison.OrdinalIgnoreCase))
                {
                    try { m_vMutexesWin[i].Value.ReleaseMutex(); }
                    catch(Exception) { Debug.Assert(false); }

                    try { m_vMutexesWin[i].Value.Close(); }
                    catch(Exception) { Debug.Assert(false); }

                    m_vMutexesWin.RemoveAt(i);
                    return true;
                }
            }

            return false;
        }

        private static bool ReleaseMutexUnix(string strName)
        {
            for (int i = 0; i < m_vMutexesUnix.Count; ++i)
            {
                if (m_vMutexesUnix[i].Key.Equals(strName, StringComparison.OrdinalIgnoreCase))
                {
                    for (int r = 0; r < 12; ++r)
                    {
                        try
                        {
                            if (!File.Exists(m_vMutexesUnix[i].Value)) break;

                            File.Delete(m_vMutexesUnix[i].Value);
                            break;
                        }
                        catch(Exception) { }

                        Thread.Sleep(10);
                    }

                    m_vMutexesUnix.RemoveAt(i);
                    return true;
                }
            }

            return false;
        }

        public static void ReleaseAll()
        {
            if (!SysInfo.IsUnix()) // Windows
            {
                for (int i = m_vMutexesWin.Count - 1; i >= 0; --i)
                    ReleaseMutexWin(m_vMutexesWin[i].Key);
            }
            else
            {
                for (int i = m_vMutexesUnix.Count - 1; i >= 0; --i)
                    ReleaseMutexUnix(m_vMutexesUnix[i].Key);
            }
        }

        public static void Refresh()
        {
            if (!SysInfo.IsUnix()) return; // Windows, no refresh required

            // Unix
            int iTicksDiff = (Environment.TickCount - m_iLastRefresh);
            if (iTicksDiff >= GmpMutexRefreshMs)
            {
                m_iLastRefresh = Environment.TickCount;

                for (int i = 0; i < m_vMutexesUnix.Count; ++i)
                {
                    try { WriteMutexFilePriv(m_vMutexesUnix[i].Value); }
                    catch(Exception) { Debug.Assert(false); }
                }
            }
        }

        private static string GetMutexPath(string strName)
        {
            string strDir = IpcBroadcast.GetTempPath();
            return (strDir + "GKIPC-" + IpcBroadcast.GetUserID() + "-Mutex-" + strName + ".tmp");
        }
    }
}

#endif