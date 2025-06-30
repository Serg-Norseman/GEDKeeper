/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih, Alex Zaytsev.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
        private const byte GS_MINOR_VER = 3;

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

        public override void LoadFromStream(Stream inputStream, bool charsetDetection = false)
        {
            byte[] gsHeader = new byte[8];
            inputStream.Read(gsHeader, 0, 8);
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
                using (var cryptoStream = new CryptoStream(inputStream, cryptic.CreateDecryptor(), CryptoStreamMode.Read)) {
                    // System.Security.Cryptography.CryptoStream -> CanSeek = false
                    // fileStream.Length is incorrect to use, because it is the length of the already encrypted file, not the original one.
                    // But it is impossible to implement saving the file size, because it is unknown until completion...
                    // or when saving a file to a crypto-stream, then move to the beginning of the file stream and rewrite the header
                    InitProgress(inputStream.Length);
                    base.LoadFromStream(cryptoStream, charsetDetection);
                }
            }
        }

        private SymmetricAlgorithm CreateCSP(byte majorVer, byte minorVer)
        {
            int blockSize;
#if NETCORE
            blockSize = 128; // .net6: BlockSize must be 128 in this implementation.
#else
            blockSize = (minorVer == 2) ? 256 : 128;
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

                    case 2:
                    case 3:
                        {
                            var keyBytes = new byte[blockSize / 8];
                            Array.Copy(pwd, keyBytes, Math.Min(keyBytes.Length, pwd.Length));
                            csp = new RijndaelManaged();
                            csp.KeySize = blockSize;
                            csp.BlockSize = blockSize;
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
