#if __MonoCS__

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Threading;
using System.Xml.Serialization;

using GKUI;

namespace Externals.SingleInstancing
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
            this.Message = string.Empty;
            this.Params = string.Empty;
        }

        public IpcParamEx(string strMessage, string strParams)
        {
            this.Message = (strMessage ?? string.Empty);
            this.Params = (strParams ?? string.Empty);
        }
    }

    public sealed class IpcMessage
    {
        public ulong ID;
        public long Time;
        public AppMessage Message;
        public int LParam;

        public static void Serialize(BinaryWriter bw, IpcMessage msg)
        {
            if((bw == null) || (msg == null)) { Debug.Assert(false); return; }

            bw.Write(msg.ID);
            bw.Write(msg.Time);
            bw.Write((int)msg.Message);
            bw.Write(msg.LParam);
        }

        public static IpcMessage Deserialize(BinaryReader br)
        {
            if(br == null) { Debug.Assert(false); return null; }

            IpcMessage msg = new IpcMessage();

            msg.ID = br.ReadUInt64();
            msg.Time = br.ReadInt64();
            msg.Message = (AppMessage)br.ReadInt32();
            msg.LParam = br.ReadInt32();

            return msg;
        }
    }

    public static class IpcBroadcast
    {
        internal static string GetUserID()
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

        public static void StartServer()
        {
            StopServer();

            FswEnsurePaths();
            try
            {
                m_fsw = new FileSystemWatcher(GetTempPath(), m_strMsgFileName);
            }
            catch(Exception) { Debug.Assert(false); return; } // Access denied

            m_fsw.IncludeSubdirectories = false;
            m_fsw.NotifyFilter = (NotifyFilters.CreationTime | NotifyFilters.LastWrite);
            m_fsw.Created += OnFileEvents;
            m_fsw.Changed += OnFileEvents;

            m_fsw.EnableRaisingEvents = true;
        }

        public static void StopServer()
        {
            if (m_fsw != null)
            {
                m_fsw.EnableRaisingEvents = false;

                m_fsw.Changed -= OnFileEvents;
                m_fsw.Created -= OnFileEvents;

                m_fsw.Dispose();
                m_fsw = null;
            }
        }

        private static string m_strMsgFilePath = null;
        private static string m_strMsgFileName = null;
        private static FileSystemWatcher m_fsw = null;
        private static object m_csProcess = new object();

        private static List<IpcMessage> m_vProcessedMsgs = new List<IpcMessage>();

        private const double IpcMsgValidSecs = 4.0;
        private const int IpcComRetryCount = 30;
        private const int IpcComRetryDelay = 10;
        private const ulong IpcFileSig = 0x038248CB851D7A7CUL;

        private static void FswEnsurePaths()
        {
            if (m_strMsgFilePath != null) return;

            m_strMsgFileName = IpcMsgFilePreID + GetUserID() + IpcMsgFilePostID;
            m_strMsgFilePath = GetTempPath() + m_strMsgFileName;
        }

        public static void Send(AppMessage msg, int lParam, bool bWaitWithTimeout)
        {
            FswEnsurePaths();

            IpcMessage ipcMsg = new IpcMessage();
            ipcMsg.ID = 0;
            ipcMsg.Time = DateTime.UtcNow.ToBinary();
            ipcMsg.Message = msg;
            ipcMsg.LParam = lParam;

            // Send just to others, not to own
            m_vProcessedMsgs.Add(ipcMsg);

            for (int r = 0; r < IpcComRetryCount; ++r)
            {
                try
                {
                    List<IpcMessage> l = ReadMessagesPriv();
                    CleanOldMessages(l);
                    l.Add(ipcMsg);

                    byte[] pbPlain;
                    using (MemoryStream ms = new MemoryStream()) {
                        using (BinaryWriter bw = new BinaryWriter(ms)) {
                            bw.Write(IpcFileSig);
                            bw.Write((uint)l.Count);

                            for (int j = 0; j < l.Count; ++j)
                                IpcMessage.Serialize(bw, l[j]);

                            pbPlain = ms.ToArray();
                        }
                    }

                    using (FileStream fsWrite = new FileStream(m_strMsgFilePath, FileMode.Create, FileAccess.Write, FileShare.None)) {
                        fsWrite.Write(pbPlain, 0, pbPlain.Length);
                    }

                    break;
                }
                catch(Exception) { }

                Thread.Sleep(IpcComRetryDelay);
            }

            CleanOldMessages(m_vProcessedMsgs);
        }

        private static void OnFileEvents(object sender, FileSystemEventArgs e)
        {
            if ((e == null) || (e.FullPath == null) || !m_strMsgFilePath.Equals(e.FullPath, StringComparison.OrdinalIgnoreCase))
            {
                Debug.Assert(false);
                return;
            }

            lock (m_csProcess)
            {
                for (int r = 0; r < IpcComRetryCount; ++r)
                {
                    try {
                        ProcessIpcMessagesPriv();
                        break;
                    }
                    catch (Exception) { }

                    Thread.Sleep(IpcComRetryDelay);
                }

                CleanOldMessages(m_vProcessedMsgs);
            }
        }

        private static void ProcessIpcMessagesPriv()
        {
            List<IpcMessage> l = ReadMessagesPriv();
            CleanOldMessages(l);

            foreach (IpcMessage msg in l)
            {
                bool bProcessed = false;
                foreach (IpcMessage ipcMsg in m_vProcessedMsgs)
                {
                    if (ipcMsg.ID == msg.ID) { bProcessed = true; break; }
                }
                if (bProcessed) continue;

                m_vProcessedMsgs.Add(msg);

                GKProgram.ProcessMessage(msg);
            }
        }

        private static List<IpcMessage> ReadMessagesPriv()
        {
            List<IpcMessage> l = new List<IpcMessage>();
            if (!File.Exists(m_strMsgFilePath)) return l;

            byte[] pbEnc = File.ReadAllBytes(m_strMsgFilePath);
            byte[] pb = pbEnc;

            MemoryStream ms = new MemoryStream(pb, false);
            BinaryReader br = new BinaryReader(ms);
            ulong uSig = br.ReadUInt64();
            if(uSig != IpcFileSig) { Debug.Assert(false); return l; }
            uint uMessages = br.ReadUInt32();

            for (uint u = 0; u < uMessages; ++u)
                l.Add(IpcMessage.Deserialize(br));

            br.Close();
            ms.Close();
            return l;
        }

        private static void CleanOldMessages(List<IpcMessage> l)
        {
            DateTime dtNow = DateTime.UtcNow;
            for (int i = l.Count - 1; i >= 0; --i)
            {
                DateTime dtEvent = DateTime.FromBinary(l[i].Time);

                if ((dtNow - dtEvent).TotalSeconds > IpcMsgValidSecs)
                    l.RemoveAt(i);
            }
        }

        internal const string IpcMsgFilePreID = "KeePassIPC-";
        internal const string IpcMsgFilePostID = "-Msgs.tmp";
        public const string CmdOpenDatabase = "OpenDatabase";

        public static string GetTempPath()
        {
            string strPath = Path.GetTempPath();

            if (!strPath.EndsWith(""+Path.DirectorySeparatorChar)) {
                strPath += Path.DirectorySeparatorChar;
            }

            return strPath;
        }

        public static string SafeSerialize(string[] args)
        {
            if (args == null) throw new ArgumentNullException("args");

            MemoryStream ms = new MemoryStream();

            XmlSerializer xml = new XmlSerializer(typeof(string[]));
            xml.Serialize(ms, args);

            string strSerialized = Convert.ToBase64String(ms.ToArray(), Base64FormattingOptions.None);
            ms.Close();
            return strSerialized;
        }

        public static string[] SafeDeserialize(string str)
        {
            if (str == null) throw new ArgumentNullException("str");

            byte[] pb = Convert.FromBase64String(str);
            MemoryStream ms = new MemoryStream(pb, false);
            XmlSerializer xml = new XmlSerializer(typeof(string[]));

            try {
                return (string[])xml.Deserialize(ms);
            } catch (Exception) {
                Debug.Assert(false);
            } finally {
                ms.Close();
            }

            return null;
        }

        public static void SendGlobalMessage(IpcParamEx ipcMsg)
        {
            if (ipcMsg == null) throw new ArgumentNullException("ipcMsg");

            Random rnd = new Random();
            int nId = rnd.Next() & 0x7FFFFFFF;

            //Console.WriteLine("nId: " + nId.ToString());
            if (WriteIpcInfoFile(nId, ipcMsg) == false) return;

            try
            {
                Send(AppMessage.IpcByFile, nId, true);
            }
            catch (Exception) { Debug.Assert(false); }

            string strIpcFile = GetIpcFilePath(nId);
            for (int r = 0; r < 50; ++r)
            {
                try {
                    if(!File.Exists(strIpcFile)) break;
                }
                catch(Exception) { }

                Thread.Sleep(20);
            }

            RemoveIpcInfoFile(nId);
        }

        private static string GetIpcFilePath(int nId)
        {
            try
            {
                return (GetTempPath() + IpcMsgFilePreID + nId.ToString() + ".tmp");
            }
            catch(Exception) { Debug.Assert(false); }

            return null;
        }

        private static bool WriteIpcInfoFile(int nId, IpcParamEx ipcMsg)
        {
            string strPath = GetIpcFilePath(nId);
            if (string.IsNullOrEmpty(strPath)) return false;

            try
            {
                //Console.WriteLine("InfoFile: " + strPath);

                XmlSerializer xml = new XmlSerializer(typeof(IpcParamEx));
                FileStream fs = new FileStream(strPath, FileMode.Create, FileAccess.Write, FileShare.None);

                try { xml.Serialize(fs, ipcMsg); }
                catch(Exception) { Debug.Assert(false); }

                fs.Close();
                return true;
            }
            catch (Exception) {
                Console.WriteLine("Error: " + strPath);
                Debug.Assert(false);
            }

            return false;
        }

        private static void RemoveIpcInfoFile(int nId)
        {
            /*string strPath = GetIpcFilePath(nId);
			if(string.IsNullOrEmpty(strPath)) return;

			try { if(File.Exists(strPath)) File.Delete(strPath); }
			catch(Exception) { Debug.Assert(false); }*/
        }

        private static IpcParamEx LoadIpcInfoFile(int nId)
        {
            string strPath = GetIpcFilePath(nId);
            if (string.IsNullOrEmpty(strPath)) return null;

            IpcParamEx ipcParam = null;
            try
            {
                XmlSerializer xml = new XmlSerializer(typeof(IpcParamEx));
                FileStream fs = new FileStream(strPath, FileMode.Open, FileAccess.Read, FileShare.Read);

                try { ipcParam = (IpcParamEx)xml.Deserialize(fs); }
                catch(Exception) { Debug.Assert(false); }

                fs.Close();
            }
            catch(Exception) { }

            RemoveIpcInfoFile(nId);

            return ipcParam;
        }

        public static void ProcessGlobalMessage(int nId, ISingleInstanceEnforcer mf)
        {
            if (mf == null)
                throw new ArgumentNullException("mf");

            IpcParamEx ipcMsg = LoadIpcInfoFile(nId);

            if (ipcMsg != null && ipcMsg.Message == CmdOpenDatabase)
            {
                string[] vArgs = SafeDeserialize(ipcMsg.Params);

                if (vArgs != null) {
                    MessageEventArgs eArgs = new MessageEventArgs(vArgs);
                    mf.OnMessageReceived(eArgs);
                }
            }
        }
        
    }
}

#endif
