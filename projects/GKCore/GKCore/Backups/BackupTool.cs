/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Options;

namespace GKCore.Backups
{
    public static class BackupTool
    {
        private static void RemoveOldestBackups(string fileName, string bakPath)
        {
            var backupOpts = GlobalOptions.Instance.Backups;

            string backupFiles = Path.GetFileName(fileName) + ".*";
            DirectoryInfo bakPathInfo = new DirectoryInfo(bakPath);
            FileInfo[] bakFiles = bakPathInfo.GetFiles(backupFiles);
            if (bakFiles.Length > backupOpts.FileBackupEachRevisionMaxCount) {
                List<Tuple<string, int>> tuples = new List<Tuple<string, int>>();
                foreach (var bak in bakFiles) {
                    try {
                        int bakVersion = Convert.ToInt32(bak.Extension.Substring(1));
                        tuples.Add(new Tuple<string, int>(bak.FullName, bakVersion));
                    } catch (Exception) {
                    }
                }
                tuples.Sort((a, b) => b.Item2.CompareTo(a.Item2));
                for (int i = backupOpts.FileBackupEachRevisionMaxCount; i < tuples.Count; i++) {
                    try {
                        File.Delete(tuples[i].Item1);
                    } catch (Exception) {
                    }
                }
            }
        }

        public static void ProcessBackup(GDMTree tree, string oldFileName, string fileName)
        {
            var backupOpts = GlobalOptions.Instance.Backups;

            switch (backupOpts.FileBackup) {
                case FileBackup.fbNone:
                    break;

                case FileBackup.fbOnlyPrev:
                    if (string.Equals(oldFileName, fileName) && File.Exists(oldFileName)) {
                        string bakFile = fileName + ".bak";
                        if (File.Exists(bakFile)) {
                            File.Delete(bakFile);
                        }

                        File.Move(oldFileName, bakFile);
                    }
                    break;

                case FileBackup.fbEachRevision:
                    if (File.Exists(fileName)) {
                        int rev = tree.Header.File.Revision;
                        string bakPath = Path.GetDirectoryName(fileName) + Path.DirectorySeparatorChar + "__history" + Path.DirectorySeparatorChar;
                        string bakFile = Path.GetFileName(fileName) + "." + ConvertHelper.AdjustNumber(rev, 3);

                        if (!Directory.Exists(bakPath)) Directory.CreateDirectory(bakPath);
                        File.Move(fileName, bakPath + bakFile);

                        if (backupOpts.FileBackupEachRevisionMaxCount > 0) RemoveOldestBackups(fileName, bakPath);
                    }
                    break;
            }
        }

        public static async Task ExtendedBackup(BaseContext baseContext, string fileName)
        {
            await Task.Run(() => {
                try {
                    var backupOpts = GlobalOptions.Instance.Backups;

                    if (backupOpts.ExtendedBackup) {
                        string pureFileName = Path.GetFileName(fileName);
                        string backupFileName = Path.Combine(backupOpts.ExtendedFolder, pureFileName);

                        baseContext.CopyFile(fileName, backupFileName, true);
                        baseContext.MoveMediaContainers(fileName, backupFileName, true);
                    }
                } catch (Exception ex) {
                    Logger.WriteError("BackupTool.ExtendedBackup()", ex);
                }
            });
        }
    }
}
