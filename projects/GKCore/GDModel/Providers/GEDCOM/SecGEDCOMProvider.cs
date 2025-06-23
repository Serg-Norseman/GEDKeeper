using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using GKCore;

namespace GDModel.Providers.GEDCOM
{
    public class SecGEDCOMProvider : GEDCOMProvider
    {
        private const string GEDSEC_HEADER = "GEDSECAA";
        private const byte GS_MAJOR_VER = 1;
        private const byte GS_MINOR_VER = 2;

        private readonly string fPassword;

        public SecGEDCOMProvider(GDMTree tree, string password)
            : base(tree)
        {
            fPassword = password;
        }

        public SecGEDCOMProvider(GDMTree tree, string password, bool keepRichNames, bool strict)
            : base(tree, keepRichNames, strict)
        {
            fPassword = password;
        }

        public override void SaveToFile(string fileName, GEDCOMCharacterSet charSet)
        {
            // Attention: processing of Header moved to BaseContext!
            using (var fileStream = new FileStream(fileName, FileMode.Create, FileAccess.Write)) {
                var gsHeader = Encoding.ASCII.GetBytes(GEDSEC_HEADER);
                gsHeader[6] = GS_MAJOR_VER;
                gsHeader[7] = GS_MINOR_VER;
                fileStream.Write(gsHeader, 0, 8);

                using (var cryptic = CreateCSP(GS_MAJOR_VER, GS_MINOR_VER))
                using (CryptoStream crStream = new CryptoStream(fileStream, cryptic.CreateEncryptor(), CryptoStreamMode.Write)) {
                    SaveToStreamExt(crStream, charSet);
                    crStream.Flush();
                }
            }
        }

        public override void LoadFromFile(string fileName, bool charsetDetection = false)
        {
            using (var fileStream = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                byte[] gsHeader = new byte[8];
                fileStream.Read(gsHeader, 0, 8);
                byte gsMajVer = gsHeader[6];
                byte gsMinVer = gsHeader[7];
                gsHeader[6] = 65;
                gsHeader[7] = 65;
                var gsh = Encoding.ASCII.GetString(gsHeader);

                if (!string.Equals(gsh, GEDSEC_HEADER)) {
                    throw new GKException(LangMan.LS(LSID.ItsNotGEDSECCompatibleFile));
                }

                if (gsMajVer < GS_MAJOR_VER || gsMinVer < GS_MINOR_VER) {
                    // dummy for future
                }

                using (var cryptic = CreateCSP(gsMajVer, gsMinVer)) {
                    using (CryptoStream crStream = new CryptoStream(fileStream, cryptic.CreateDecryptor(), CryptoStreamMode.Read)) {
                        LoadFromStreamExt(fileStream, crStream, charsetDetection);
                    }
                }
            }
        }

        private SymmetricAlgorithm CreateCSP(byte majorVer, byte minorVer)
        {
#if NETCORE
            const int BlockSize = 128;
#else
            const int BlockSize = 256;
#endif

            if (majorVer >= 1) {
                SymmetricAlgorithm csp = null;

                byte[] pwd = Encoding.Unicode.GetBytes(fPassword);

                switch (minorVer) {
                    case 1: {
                        byte[] salt = SCCrypt.CreateRandomSalt(7);
                        csp = new DESCryptoServiceProvider();
                        var pdb = new PasswordDeriveBytes(pwd, salt);
                        try {
                            csp.Key = pdb.CryptDeriveKey("DES", "SHA1", csp.KeySize, csp.IV);
                            SCCrypt.ClearBytes(salt);
                        } finally {
                            var pdbDisp = pdb as IDisposable;
                            if (pdbDisp != null) pdbDisp.Dispose();
                        }
                    }
                        break;

                    case 2: {
                        var keyBytes = new byte[BlockSize / 8];
                        Array.Copy(pwd, keyBytes, Math.Min(keyBytes.Length, pwd.Length));
                        csp = new RijndaelManaged();
                        csp.KeySize = BlockSize;
                        csp.BlockSize = BlockSize;
                        csp.Key = keyBytes;
                        csp.IV = keyBytes;
                        csp.Padding = PaddingMode.PKCS7;
                        csp.Mode = CipherMode.CBC;
                    }
                        break;
                }

                SCCrypt.ClearBytes(pwd);

                return csp;
            }

            return null;
        }
    }
}
