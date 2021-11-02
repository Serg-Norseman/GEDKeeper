/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;

namespace GKMap
{
    /// <summary>
    /// etc functions...
    /// </summary>
    internal class Stuff
    {
        public static string EnumToString(Enum value)
        {
            FieldInfo fi = value.GetType().GetField(value.ToString());
            DescriptionAttribute[] attributes = (DescriptionAttribute[])fi.GetCustomAttributes(typeof(DescriptionAttribute), false);
            return (attributes.Length > 0) ? attributes[0].Description : value.ToString();
        }

        public static readonly Random Random = new Random();

        public static string LoadResourceString(string resName)
        {
            Assembly assembly = typeof(Stuff).Assembly;
            Stream resStream = assembly.GetManifestResourceStream(resName);
            string result;
            using (StreamReader sr = new StreamReader(resStream)) {
                result = sr.ReadToEnd();
            }
            return result;
        }

        public static Stream LoadResourceStream(string resName)
        {
            Assembly assembly = typeof(Stuff).Assembly;
            Stream resStream = assembly.GetManifestResourceStream(resName);
            return resStream;
        }

        public static MemoryStream CopyStream(Stream inputStream, bool seekOriginBegin)
        {
            const int readSize = 32 * 1024;
            byte[] buffer = new byte[readSize];
            MemoryStream ms = new MemoryStream();
            {
                int count;
                while ((count = inputStream.Read(buffer, 0, readSize)) > 0) {
                    ms.Write(buffer, 0, count);
                }
            }
            if (seekOriginBegin) {
                inputStream.Seek(0, SeekOrigin.Begin);
            }
            ms.Seek(0, SeekOrigin.Begin);
            return ms;
        }

        public static bool IsRunningOnWin7orLater()
        {
            OperatingSystem os = Environment.OSVersion;

            if (os.Platform == PlatformID.Win32NT) {
                Version vs = os.Version;

                if (vs.Major >= 6 && vs.Minor > 0) {
                    return true;
                }
            }

            return false;
        }

        private static string EncryptString(string message, string passphrase)
        {
            byte[] results;

            using (var hashProvider = new SHA1CryptoServiceProvider()) {
                byte[] tdesKey = hashProvider.ComputeHash(Encoding.UTF8.GetBytes(passphrase));
                Array.Resize(ref tdesKey, 16);

                using (TripleDESCryptoServiceProvider tdesAlgorithm = new TripleDESCryptoServiceProvider()) {
                    tdesAlgorithm.Key = tdesKey;
                    tdesAlgorithm.Mode = CipherMode.ECB;
                    tdesAlgorithm.Padding = PaddingMode.PKCS7;

                    byte[] dataToEncrypt = Encoding.UTF8.GetBytes(message);

                    // Step 5. Attempt to encrypt the string
                    try {
                        using (ICryptoTransform encryptor = tdesAlgorithm.CreateEncryptor()) {
                            results = encryptor.TransformFinalBlock(dataToEncrypt, 0, dataToEncrypt.Length);
                        }
                    } finally {
                        // Clear the TripleDes and Hashprovider services of any sensitive information
                        tdesAlgorithm.Clear();
                        hashProvider.Clear();
                    }
                }
            }

            // Step 6. Return the encrypted string as a base64 encoded string
            return Convert.ToBase64String(results);
        }

        private static string DecryptString(string message, string passphrase)
        {
            byte[] results;

            using (var hashProvider = new SHA1CryptoServiceProvider()) {
                byte[] tdesKey = hashProvider.ComputeHash(Encoding.UTF8.GetBytes(passphrase));
                Array.Resize(ref tdesKey, 16);

                // Step 2. Create a new TripleDESCryptoServiceProvider object
                using (TripleDESCryptoServiceProvider tdesAlgorithm = new TripleDESCryptoServiceProvider()) {
                    // Step 3. Setup the decoder
                    tdesAlgorithm.Key = tdesKey;
                    tdesAlgorithm.Mode = CipherMode.ECB;
                    tdesAlgorithm.Padding = PaddingMode.PKCS7;

                    // Step 4. Convert the input string to a byte[]
                    byte[] dataToDecrypt = Convert.FromBase64String(message);

                    // Step 5. Attempt to decrypt the string
                    try {
                        using (ICryptoTransform decryptor = tdesAlgorithm.CreateDecryptor()) {
                            results = decryptor.TransformFinalBlock(dataToDecrypt, 0, dataToDecrypt.Length);
                        }
                    } finally {
                        // Clear the TripleDes and Hashprovider services of any sensitive information
                        tdesAlgorithm.Clear();
                        hashProvider.Clear();
                    }
                }
            }

            // Step 6. Return the decrypted string in UTF8 format
            return Encoding.UTF8.GetString(results, 0, results.Length);
        }

        public static string GString(string message)
        {
            return DecryptString(message, Manifesto);
        }

        private static readonly string Manifesto = "GMap.NET is great and Powerful, Free, cross platform, open source .NET control.";

        public static string GetApplicationDataFolderPath()
        {
            bool isSystem = false;
            try {
                using (var identity = System.Security.Principal.WindowsIdentity.GetCurrent()) {
                    isSystem = identity.IsSystem;
                }
            } catch (Exception ex) {
                Trace.WriteLine("SQLitePureImageCache, WindowsIdentity.GetCurrent: " + ex);
            }

            var specFolder = (isSystem) ? Environment.SpecialFolder.CommonApplicationData : Environment.SpecialFolder.LocalApplicationData;
            string path = Environment.GetFolderPath(specFolder);

            if (!string.IsNullOrEmpty(path)) {
                path += Path.DirectorySeparatorChar + "GKMap" + Path.DirectorySeparatorChar;
            }

            return path;
        }
    }
}
