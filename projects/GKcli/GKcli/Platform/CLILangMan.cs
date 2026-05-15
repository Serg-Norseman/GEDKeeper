/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Locales;

namespace GKUI.Platform;

public enum CLS
{
    SelectCommand = 1,
    Answers = 2,
    ConfirmError = 3,
}


public static class CLILangMan
{
    private static ILangMan fInstance;

    public static ILangMan Instance
    {
        get { return fInstance; }
        internal set { fInstance = value; }
    }

    public static string LS(Enum lsid)
    {
        return fInstance == null ? string.Empty : fInstance.LS(lsid);
    }
}
