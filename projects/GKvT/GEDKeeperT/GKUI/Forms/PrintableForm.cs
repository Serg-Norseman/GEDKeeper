/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Graphics;

namespace GKUI.Forms
{
    /// <summary>
    /// Form's class, common for the implementation of the print.
    /// </summary>
    public class PrintableForm : StatusForm
    {
        protected PrintableForm()
        {
        }

        protected virtual IPrintable GetPrintable()
        {
            return null;
        }

        public bool AllowPrint()
        {
            return false;
        }

        public void DoPrint()
        {
            // Not supported
        }

        public void DoPrintPreview()
        {
            // Not supported
        }
    }
}
