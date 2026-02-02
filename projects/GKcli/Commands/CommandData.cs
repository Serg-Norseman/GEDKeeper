/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKUI.Commands;

internal delegate void CommandFunc(BaseContext baseContext, object obj);

internal enum CommandCategory
{
    Application,
    File,

    Individual,
    Family,

    Note,
    Multimedia,
    Source,
    Repository
}

internal class CommandData
{
    public string Command { get; private set; }
    public CommandCategory Category { get; private set; }
    public CommandFunc Func { get; private set; }

    public CommandData(string command, CommandCategory category, CommandFunc func)
    {
        Command = command;
        Category = category;
        Func = func;
    }
}
