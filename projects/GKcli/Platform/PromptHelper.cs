/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Sharprompt;

namespace GKUI.Platform;

internal static class PromptHelper
{
    internal static void WriteLine()
    {
        Console.WriteLine();
    }

    internal static void WriteLine(string value)
    {
        WriteMarkupLine(value);
    }

    internal static void WriteLine(int indent, string value)
    {
        var strIndent = new string(' ', indent * 2);
        WriteMarkupLine(strIndent + value);
    }

    internal static void WriteLine(string value, params object[] args)
    {
        WriteMarkupLine(string.Format(value, args));
    }

    internal static void WriteLine(int indent, string value, params object[] args)
    {
        var strIndent = new string(' ', indent * 2);
        WriteMarkupLine(strIndent + string.Format(value, args));
    }

    public static string SelectFile(string initialPath, params string[] extensions)
    {
        initialPath = initialPath.TrimEnd('/').TrimEnd('\\');
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

    public static string SelectFolder(string initialPath)
    {
        initialPath = initialPath.TrimEnd('/').TrimEnd('\\');
        var currentDir = Path.GetFullPath(initialPath);

        while (true) {
            var dirs = Directory.GetDirectories(currentDir)
                .Select(d => $"{Path.GetFileName(d)}/");

            var parentDirInfo = Directory.GetParent(currentDir);

            var options = dirs.ToList();

            if (parentDirInfo != null)
                options.Insert(0, "..");

            options.Insert(0, ".");

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

    public static void WriteMarkupLine(string text)
    {
        var parts = Regex.Split(text, @"(\[[a-zA-Z]+\]|\[/\])");
        var defaultColor = Console.ForegroundColor;

        foreach (var part in parts) {
            if (part == "[/]") {
                Console.ResetColor();
            } else if (part.StartsWith("[") && part.EndsWith("]")) {
                string colorName = part.Trim('[', ']');
                if (Enum.TryParse(colorName, true, out ConsoleColor newColor))
                    Console.ForegroundColor = newColor;
            } else {
                Console.Write(part);
            }
        }
        Console.ResetColor();
        Console.WriteLine();
    }

    public static bool GetConfirm(string message, char chY, char chN, string errorMsg)
    {
        var answer = Prompt.Input<char>($"{message} ({chY}/{chN})?", defaultValue: chY,
            validators: new[] {
                Validators.Required(),
                value => {
                    char val = (value is char sym) ? char.ToLower(sym) : (char)0;

                    chY = char.ToLower(chY);
                    chN = char.ToLower(chN);
                    if (val == chY || val == chN) {
                        return ValidationResult.Success;
                    } else {
                        return new ValidationResult(string.Format(errorMsg, chY, chN));
                    }
                }
            }
        );

        return answer == chY;
    }
}
