/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Linq;
using Sharprompt;

namespace GKUI.Platform;

internal static class PromptHelper
{
    public static string SelectFile(string initialPath, params string[] extensions)
    {
        var currentDir = Path.GetFullPath(initialPath);

        while (true) {
            var dirs = Directory.GetDirectories(currentDir)
                .Select(d => $"{Path.GetFileName(d)}/");

            var files = Directory.GetFiles(currentDir)
                .Where(f => extensions.Contains(Path.GetExtension(f).ToLower()))
                .Select(f => Path.GetFileName(f));

            var parentDirInfo = Directory.GetParent(currentDir);

            var options = dirs.Concat(files).ToList();

            if (parentDirInfo != null)
                options.Insert(0, "..");

            var selected = Prompt.Select($"Current: {currentDir}", options);

            if (selected == "..") {
                currentDir = parentDirInfo?.FullName ?? currentDir;
            } else if (selected.EndsWith("/")) {
                currentDir = Path.Combine(currentDir, selected.Replace("/", ""));
            } else {
                return Path.Combine(currentDir, selected);
            }
        }
    }
}
