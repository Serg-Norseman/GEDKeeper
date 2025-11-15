#if !NETSTANDARD

using System;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Reflection;
using System.Security.Principal;
using SQLite;

namespace GKMap.CacheProviders
{
    internal static class SQLiteHelper
    {
        public static void Load()
        {
            string asmPath = GetAssemblyPath();
            string targetFile = asmPath + "sqlite3.dll";
            if (!File.Exists(targetFile)) {
                var isAdmin = new WindowsPrincipal(WindowsIdentity.GetCurrent()).IsInRole(WindowsBuiltInRole.Administrator);

                var programFilesPaths = new[] {
                    Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles),
                    Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86)
                };
                var isProgramFiles = programFilesPaths.Any(pf => asmPath.StartsWith(pf, StringComparison.OrdinalIgnoreCase));

                if (!isProgramFiles || isAdmin) {
                    ExtractResource(targetFile);
                } else {
                    //MessageBox.Show("Необходимы права администратора для установки компонента `sqlite3`.");

                    string dllCache = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) + Path.DirectorySeparatorChar + "GKMap" + Path.DirectorySeparatorChar;
                    if (!Directory.Exists(dllCache))
                        Directory.CreateDirectory(dllCache);

                    dllCache += "sqlite3.dll";
                    if (!File.Exists(dllCache))
                        ExtractResource(dllCache);

                    CopyWithElevatedRights(dllCache, targetFile);
                }
            }
        }

        private static void ExtractResource(string targetFile)
        {
            string gzName = IntPtr.Size == 8 ? "sqlite3.x64.dll.gz" : "sqlite3.x86.dll.gz";
            Assembly assembly = typeof(SQLiteLoader).Assembly;
            Stream resStream = assembly.GetManifestResourceStream("Resources." + gzName);

            using (var gs = new GZipStream(resStream, CompressionMode.Decompress)) {
                using (var exctDll = new FileStream(targetFile, FileMode.CreateNew)) {
                    byte[] tmp = new byte[1024 * 256];
                    int r;
                    while ((r = gs.Read(tmp, 0, tmp.Length)) > 0) {
                        exctDll.Write(tmp, 0, r);
                    }
                }
            }
        }

        private static string GetAssemblyPath()
        {
            Assembly asm = Assembly.GetEntryAssembly();
            if (asm == null) {
                asm = Assembly.GetExecutingAssembly();
            }

            Module[] mods = asm.GetModules();
            string fn = mods[0].FullyQualifiedName;
            return Path.GetDirectoryName(fn) + Path.DirectorySeparatorChar;
        }

        private static bool CopyWithElevatedRights(string sourcePath, string destinationPath)
        {
            try {
                var startInfo = new ProcessStartInfo {
                    FileName = "cmd.exe",
                    Arguments = $"/c copy \"{sourcePath}\" \"{destinationPath}\"",
                    Verb = "runas",
                    UseShellExecute = true,
                    WindowStyle = ProcessWindowStyle.Hidden
                };

                using (var process = Process.Start(startInfo)) {
                    process.WaitForExit();
                    return process.ExitCode == 0;
                }
            } catch (Exception) {
                return false;
            }
        }

        /*
        public static bool HasAdminAccess(string path)
        {
            try {
                var directoryInfo = new DirectoryInfo(path);
                var accessRule = directoryInfo.GetAccessControl();
            
                var currentIdentity = WindowsIdentity.GetCurrent();
                var principal = new WindowsPrincipal(currentIdentity);
            
                return principal.IsInRole(WindowsBuiltInRole.Administrator);
            } catch (UnauthorizedAccessException) {
                return false;
            } catch (Exception ex) {
                return false;
            }
        }

        public static void RequestAccess(string path)
        {
            if (!HasAdminAccess(path)) {
                ProcessStartInfo startInfo = new ProcessStartInfo();
                startInfo.UseShellExecute = true;
                startInfo.WorkingDirectory = Path.GetDirectoryName(path);
                startInfo.FileName = System.Reflection.Assembly.GetExecutingAssembly().Location;
                startInfo.Verb = "runas";
                Process.Start(startInfo);
                Environment.Exit(0);
            }
        }
        */
    }
}

#endif
