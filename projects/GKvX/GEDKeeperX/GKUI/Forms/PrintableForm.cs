/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;
using GKCore.Design.Graphics;

namespace GKUI.Forms
{
    /// <summary>
    /// Form's class, common for the implementation of the print.
    /// </summary>
    public class PrintableForm<TView, TController> : StatusForm<TView, TController>
        where TView : IView
        where TController : FormController<TView>
    {
        protected PrintableForm()
        {
        }

        #region Public interface

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
        }

        public void DoPrintPreview()
        {
        }

        #endregion
    }
}
