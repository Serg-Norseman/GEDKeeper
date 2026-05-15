/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Text;
using GKCore.Design.Controls;

namespace GKUI.Platform;

internal sealed class TextOutput : ITextOutput
{
    private StringBuilder fText = new StringBuilder();

    public void AppendText(string text)
    {
        fText.Append(text);
    }

    public void Clear()
    {
        fText.Clear();
    }

    public override string ToString()
    {
        return fText.ToString();
    }
}
