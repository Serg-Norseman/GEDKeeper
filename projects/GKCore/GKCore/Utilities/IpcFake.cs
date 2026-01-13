/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Threading;
using System.Xml.Serialization;

namespace GKCore.Utilities
{
    public enum AppMessage
    {
        None = 0,
        RestoreWindow = 1,
        IpcByFile = 2
    }

    public sealed class IpcParamEx
    {
        public string Message { get; set; }
        public string Params { get; set; }

        public IpcParamEx()
        {
            Message = string.Empty;
            Params = string.Empty;
        }

        public IpcParamEx(string strMessage, string strParams)
        {
            Message = (strMessage ?? string.Empty);
            Params = (strParams ?? string.Empty);
        }
    }

    public sealed class IpcMessage
    {
        public long ID;
        public long Time;
        public AppMessage Message;
        public int LParam;

        public void Serialize(BinaryWriter bw)
        {
            if (bw == null)
                throw new ArgumentNullException(nameof(bw));

            bw.Write(ID);
            bw.Write(Time);
            bw.Write((int)Message);
            bw.Write(LParam);
        }

        public static IpcMessage Deserialize(BinaryReader br)
        {
            if (br == null)
                throw new ArgumentNullException(nameof(br));

            IpcMessage msg = new IpcMessage();
            msg.ID = br.ReadInt64();
            msg.Time = br.ReadInt64();
            msg.Message = (AppMessage)br.ReadInt32();
            msg.LParam = br.ReadInt32();

            return msg;
        }
    }

    public static class IpcFake
    {
        private static ISingleInstanceEnforcer fEnforcer;

        private static string fMsgFilePath = null;
        private static string fMsgFileName = null;
        private static FileSystemWatcher fFileWatcher = null;
        private static object fProcessLock = new object();
        private static List<IpcMessage> fProcessedMsgs = new List<IpcMessage>();

        private const double IpcMsgValidSecs = 4.0;
        private const int IpcComRetryCount = 30;
        private const int IpcComRetryDelay = 10;
        private const ulong IpcFileSig = 0x038248CB851D7A7CUL;

        private const string IpcMsgFilePreID = "GKIPC-";
        private const string IpcMsgFilePostID = "-Msgs.tmp";

        public const string CmdSendArgs = "SendArgs";

        private const double GmpMutexValidSecs = 190.0;
        private const int GmpMutexRefreshMs = 60 * 1000;

        private static List<KeyValuePair<string, string>> fMutexes = new List<KeyValuePair<string, string>>();
        private static int fLastRefresh = 0;


        public static void StartServer(ISingleInstanceEnforcer enforcer)
        {
            StopServer();
            fEnforcer = enforcer;
            EnsurePaths();

            try {
                fFileWatcher = new FileSystemWatcher(GetIpcPath(), fMsgFileName);
                fFileWatcher.IncludeSubdirectories = false;
                fFileWatcher.NotifyFilter = (NotifyFilters.CreationTime | NotifyFilters.LastWrite);
                fFileWatcher.Created += OnFileEvents;
                fFileWatcher.Changed += OnFileEvents;
                fFileWatcher.EnableRaisingEvents = true;
            } catch (Exception ex) {
                // Access denied
                Logger.WriteError("IpcFake.StartServer()", ex);
            }
        }

        public static void StopServer()
        {
            if (fFileWatcher != null) {
                fFileWatcher.EnableRaisingEvents = false;
                fFileWatcher.Changed -= OnFileEvents;
                fFileWatcher.Created -= OnFileEvents;
                fFileWatcher.Dispose();
                fFileWatcher = null;
            }
        }

        public static void Send(AppMessage msg, int lParam, bool bWaitWithTimeout)
        {
            try {
                EnsurePaths();

                IpcMessage ipcMsg = new IpcMessage();
                ipcMsg.ID = 0;
                ipcMsg.Time = DateTime.UtcNow.ToBinary();
                ipcMsg.Message = msg;
                ipcMsg.LParam = lParam;

                // Send just to others, not to own
                fProcessedMsgs.Add(ipcMsg);

                for (int r = 0; r < IpcComRetryCount; ++r) {
                    try {
                        List<IpcMessage> list = ReadMessagesPriv();
                        CleanOldMessages(list);
                        list.Add(ipcMsg);

                        byte[] pbPlain;
                        using (MemoryStream ms = new MemoryStream()) {
                            using (BinaryWriter bw = new BinaryWriter(ms)) {
                                bw.Write(IpcFileSig);
                                bw.Write((uint)list.Count);

                                for (int j = 0; j < list.Count; ++j) list[j].Serialize(bw);

                                pbPlain = ms.ToArray();
                            }
                        }

                        using (FileStream fsWrite = new FileStream(fMsgFilePath, FileMode.Create, FileAccess.Write, FileShare.None)) {
                            fsWrite.Write(pbPlain, 0, pbPlain.Length);
                        }

                        break;
                    } catch (Exception ex) {
                        Logger.WriteError("IpcFake.Send.2()", ex);
                    }

                    Thread.Sleep(IpcComRetryDelay);
                }

                CleanOldMessages(fProcessedMsgs);
            } catch (Exception ex) {
                Logger.WriteError("IpcFake.Send()", ex);
            }
        }

        public static void SendMessage(string cmd, string[] args)
        {
            IpcParamEx ipcMsg = new IpcParamEx(cmd, IpcFake.SafeSerialize(args));
            IpcFake.SendMessage(ipcMsg);
        }

        public static void SendMessage(IpcParamEx ipcMsg)
        {
            if (ipcMsg == null)
                throw new ArgumentNullException(nameof(ipcMsg));

            try {
                Random rnd = new Random();
                int nId = rnd.Next() & 0x7FFFFFFF;

                if (!WriteIpcInfoFile(nId, ipcMsg)) return;

                Send(AppMessage.IpcByFile, nId, true);

                string strIpcFile = GetIpcFilePath(nId);
                for (int r = 0; r < 50; ++r) {
                    if (!File.Exists(strIpcFile)) break;

                    Thread.Sleep(20);
                }

                RemoveIpcInfoFile(nId);
            } catch (Exception ex) {
                Logger.WriteError("IpcFake.SendMessage()", ex);
            }
        }

        public static string SafeSerialize(string[] args)
        {
            if (args == null)
                throw new ArgumentNullException(nameof(args));

            string result = null;

            using (MemoryStream ms = new MemoryStream()) {
                XmlSerializer xml = new XmlSerializer(typeof(string[]));
                xml.Serialize(ms, args);
                result = Convert.ToBase64String(ms.ToArray(), Base64FormattingOptions.None);
            }

            return result;
        }

        public static string[] SafeDeserialize(string str)
        {
            if (str == null)
                throw new ArgumentNullException(nameof(str));

            try {
                byte[] pb = Convert.FromBase64String(str);

                using (MemoryStream ms = new MemoryStream(pb, false)) {
                    XmlSerializer xml = new XmlSerializer(typeof(string[]));

                    return (string[])xml.Deserialize(ms);
                }
            } catch (Exception ex) {
                Logger.WriteError("IpcFake.SafeDeserialize()", ex);
                return null;
            }
        }

        public static string GetMutexFileName(string strName)
        {
            string strDir = GetIpcPath();
            return (strDir + IpcMsgFilePreID + GetUserID() + "-MTX-" + strName + ".tmp");
        }

        public static bool CreateMutex(string strName, bool bInitiallyOwned)
        {
            string mtxFileName = IpcFake.GetMutexFileName(strName);

            try {
                if (File.Exists(mtxFileName)) {
                    byte[] pbEnc = File.ReadAllBytes(mtxFileName);

                    if (pbEnc.Length == 12) {
                        long lTime = BitConverter.ToInt64(pbEnc, 0);
                        DateTime dt = DateTime.FromBinary(lTime);

                        if ((DateTime.UtcNow - dt).TotalSeconds < GmpMutexValidSecs) {
                            int pid = BitConverter.ToInt32(pbEnc, 8);
                            try {
                                Process.GetProcessById(pid); // Throws if process is not running
                                return false; // Actively owned by other process
                            } catch (Exception) {
                            }
                        }

                        // Release the old mutex since process is not running
                        ReleaseMutex(strName);
                    } else {
                        Debug.Assert(false);
                    }
                }
            } catch (Exception) {
                Debug.Assert(false);
            }

            try {
                WriteMutexFilePriv(mtxFileName);
            } catch (Exception) {
                Debug.Assert(false);
            }

            fMutexes.Add(new KeyValuePair<string, string>(strName, mtxFileName));
            return true;
        }

        public static bool ReleaseMutex(string strName)
        {
            for (int i = 0; i < fMutexes.Count; ++i) {
                if (fMutexes[i].Key.Equals(strName, StringComparison.OrdinalIgnoreCase)) {
                    string mtx = fMutexes[i].Value;

                    for (int r = 0; r < 12; ++r) {
                        try {
                            if (File.Exists(mtx)) {
                                File.Delete(mtx);
                            }
                            break;
                        } catch {
                        }

                        Thread.Sleep(10);
                    }

                    fMutexes.RemoveAt(i);
                    return true;
                }
            }
            return false;
        }

        public static void ReleaseAllMutexes()
        {
            for (int i = fMutexes.Count - 1; i >= 0; --i)
                ReleaseMutex(fMutexes[i].Key);
        }

        public static void RefreshMutexes()
        {
            int iTicksDiff = (Environment.TickCount - fLastRefresh);
            if (iTicksDiff >= GmpMutexRefreshMs) {
                fLastRefresh = Environment.TickCount;

                for (int i = 0; i < fMutexes.Count; ++i) {
                    try {
                        string mtx = fMutexes[i].Value;
                        WriteMutexFilePriv(mtx);
                    } catch (Exception) {
                        Debug.Assert(false);
                    }
                }
            }
        }

        #region Private methods

        private static string GetUserID()
        {
            string strID = (Environment.UserName ?? string.Empty) + @" @ " + (Environment.MachineName ?? string.Empty);
            byte[] pbID = Encoding.UTF8.GetBytes(strID);

            SHA1Managed sha1 = new SHA1Managed();
            byte[] pbHash = sha1.ComputeHash(pbID);

            string strShort = Convert.ToBase64String(pbHash, Base64FormattingOptions.None);
            strShort = strShort.Replace(@"+", string.Empty);
            strShort = strShort.Replace(@"/", string.Empty);
            if (strShort.Length > 8) strShort = strShort.Substring(0, 8);

            return strShort;
        }

        private static void OnFileEvents(object sender, FileSystemEventArgs e)
        {
            if ((e == null) || (e.FullPath == null) || !fMsgFilePath.Equals(e.FullPath, StringComparison.OrdinalIgnoreCase))
            {
                Debug.Assert(false);
                return;
            }

            lock (fProcessLock) {
                for (int r = 0; r < IpcComRetryCount; ++r) {
                    try {
                        ProcessIpcMessagesPriv();
                        break;
                    } catch (Exception) {
                    }

                    Thread.Sleep(IpcComRetryDelay);
                }

                CleanOldMessages(fProcessedMsgs);
            }
        }

        private static void ProcessIpcMessagesPriv()
        {
            List<IpcMessage> l = ReadMessagesPriv();
            CleanOldMessages(l);

            foreach (IpcMessage msg in l) {
                bool bProcessed = false;
                foreach (IpcMessage ipcMsg in fProcessedMsgs) {
                    if (ipcMsg.ID == msg.ID) {
                        bProcessed = true;
                        break;
                    }
                }

                if (bProcessed) continue;

                fProcessedMsgs.Add(msg);

                ProcessMessage(msg);
            }
        }

        private static void ProcessMessage(IpcMessage msg)
        {
            try {
                if (fEnforcer == null) return;

                if (msg.Message == AppMessage.RestoreWindow) {
                    MessageEventArgs eArgs = new MessageEventArgs("restore");
                    fEnforcer.OnMessageReceived(eArgs);
                } else if (msg.Message == AppMessage.IpcByFile) {
                    IpcParamEx ipcMsg = LoadIpcInfoFile(msg.LParam);
                    if (ipcMsg != null && ipcMsg.Message == CmdSendArgs) {
                        string[] vArgs = SafeDeserialize(ipcMsg.Params);

                        if (vArgs != null) {
                            MessageEventArgs eArgs = new MessageEventArgs(vArgs);
                            fEnforcer.OnMessageReceived(eArgs);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("IpcFake.ProcessMessage()", ex);
            }
        }

        private static List<IpcMessage> ReadMessagesPriv()
        {
            List<IpcMessage> list = new List<IpcMessage>();
            if (!File.Exists(fMsgFilePath)) return list;

            byte[] pbEnc = File.ReadAllBytes(fMsgFilePath);

            using (MemoryStream ms = new MemoryStream(pbEnc, false)) {
                using (BinaryReader br = new BinaryReader(ms)) {
                    ulong uSig = br.ReadUInt64();
                    if (uSig != IpcFileSig) {
                        Debug.Assert(false);
                        return list;
                    }

                    uint uMessages = br.ReadUInt32();
                    for (uint u = 0; u < uMessages; ++u)
                        list.Add(IpcMessage.Deserialize(br));
                }
            }

            return list;
        }

        private static void CleanOldMessages(IList<IpcMessage> list)
        {
            DateTime dtNow = DateTime.UtcNow;
            for (int i = list.Count - 1; i >= 0; --i) {
                DateTime dtEvent = DateTime.FromBinary(list[i].Time);

                if ((dtNow - dtEvent).TotalSeconds > IpcMsgValidSecs) list.RemoveAt(i);
            }
        }

        private static string GetIpcPath()
        {
            string strPath = AppHost.GetAppDataPathStatic();
            return strPath;
        }

        private static void EnsurePaths()
        {
            if (!string.IsNullOrEmpty(fMsgFilePath)) return;

            fMsgFileName = IpcMsgFilePreID + GetUserID() + IpcMsgFilePostID;
            fMsgFilePath = GetIpcPath() + fMsgFileName;
        }

        private static string GetIpcFilePath(int nId)
        {
            try {
                string filePath = GetIpcPath() + IpcMsgFilePreID + nId.ToString() + ".tmp";
                return filePath;
            } catch (Exception ex) {
                Logger.WriteError("IpcFake.GetIpcFilePath()", ex);
                return null;
            }
        }

        private static bool WriteIpcInfoFile(int nId, IpcParamEx ipcMsg)
        {
            string strPath = GetIpcFilePath(nId);
            if (string.IsNullOrEmpty(strPath)) return false;

            try {
                XmlSerializer xml = new XmlSerializer(typeof(IpcParamEx));

                using (var fs = new FileStream(strPath, FileMode.Create, FileAccess.Write, FileShare.None)) {
                    xml.Serialize(fs, ipcMsg);
                }

                return true;
            } catch (Exception ex) {
                Logger.WriteError("IpcFake.WriteIpcInfoFile()", ex);
            }

            return false;
        }

        private static void RemoveIpcInfoFile(int nId)
        {
            string strPath = GetIpcFilePath(nId);
            if (string.IsNullOrEmpty(strPath)) return;

            try {
                if (File.Exists(strPath)) File.Delete(strPath);
            } catch (Exception) {
                Debug.Assert(false);
            }
        }

        private static IpcParamEx LoadIpcInfoFile(int nId)
        {
            string strPath = GetIpcFilePath(nId);
            if (string.IsNullOrEmpty(strPath)) return null;

            IpcParamEx ipcParam = null;
            try {
                XmlSerializer xml = new XmlSerializer(typeof(IpcParamEx));

                using (var fs = new FileStream(strPath, FileMode.Open, FileAccess.Read, FileShare.Read)) {
                    ipcParam = (IpcParamEx)xml.Deserialize(fs);
                }
            } catch (Exception ex) {
                Logger.WriteError("IpcFake.LoadIpcInfoFile()", ex);
            }

            RemoveIpcInfoFile(nId);

            return ipcParam;
        }

        private static void WriteMutexFilePriv(string strPath)
        {
            byte[] pb = new byte[12];
            BitConverter.GetBytes(DateTime.UtcNow.ToBinary()).CopyTo(pb, 0);
            BitConverter.GetBytes(Process.GetCurrentProcess().Id).CopyTo(pb, 8);
            File.WriteAllBytes(strPath, pb);
        }

        #endregion
    }
}
