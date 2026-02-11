/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;

namespace GKcli.Commands;

/// <summary>
/// Abstract base class for commands.
/// </summary>
internal abstract class BaseCommand
{
    private readonly CommandCategory fCategory;
    private readonly string fSign;
    private readonly string fText;

    /// <summary>
    /// Gets the category of the command.
    /// </summary>
    public CommandCategory Category
    {
        get { return fCategory; }
    }

    /// <summary>
    /// Gets the internal identifier of the command.
    /// </summary>
    public string Sign
    {
        get { return fSign; }
    }

    /// <summary>
    /// Gets the localized identifier (LSID) of the command.
    /// </summary>
    public string Text
    {
        get { return fText; }
    }

    /// <summary>
    /// Constructor for the command.
    /// </summary>
    /// <param name="sign">Internal identifier of the command.</param>
    /// <param name="localizedID">Localized identifier (LSID) of the command.</param>
    /// <param name="category">Category of the command.</param>
    protected BaseCommand(string sign, string text, CommandCategory category)
    {
        fSign = sign;
        fText = text;
        fCategory = category;
    }

    /// <summary>
    /// Abstract method for command execution.
    /// </summary>
    /// <param name="baseContext">Base context for the command.</param>
    /// <param name="obj">Additional object parameter.</param>
    public abstract void Execute(BaseContext baseContext, object obj);
}
