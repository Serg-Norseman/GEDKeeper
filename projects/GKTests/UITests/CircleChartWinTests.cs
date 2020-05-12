/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#if !__MonoCS__

using System;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Forms;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class CircleChartWinTests : CustomWindowTest
    {

        #region Handlers for external tests

        public static void CircleChartWin_Tests(CustomWindowTest formTest, Form frm, string stage)
        {
            Assert.IsInstanceOf(typeof(CircleChartWin), frm, stage);

            CircleChartWin ccWin = frm as CircleChartWin;

            IBaseWindow curBase = ccWin.Base;
            Assert.IsNotNull(curBase);

            ccWin.UpdateSettings();

            Assert.IsFalse(ccWin.AllowFilter());
            Assert.IsFalse(ccWin.AllowQuickSearch());
            Assert.IsTrue(ccWin.AllowPrint());

            // forced update
            ccWin.Refresh();

            Assert.IsFalse(ccWin.NavCanBackward());
            ccWin.NavPrev();
            Assert.IsFalse(ccWin.NavCanForward());
            ccWin.NavNext();

            var ctl = new ControlTester("fCircleChart", frm);
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Add));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Subtract));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Left));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Back));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Right));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.D0 | Keys.Control));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Up));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Down));

            ctl.FireEvent("DoubleClick", new EventArgs());
            ctl.Properties.Refresh();
            ctl.FireEvent("DoubleClick", new EventArgs());

            // empty methods
            Assert.IsNotNull(ccWin.FindAll(""));
            ccWin.QuickSearch();
            ccWin.SelectByRec(null);
            ccWin.SetFilter();

            formTest.ModalFormHandler = SaveFileJPG_Handler;
            ClickToolStripButton("tbImageSave", ccWin);

            // FIXME exception!
            //ModalFormHandler = SaveFileEMF_Handler;
            //ClickToolStripButton("tbImageSave", ccWin);

            formTest.ModalFormHandler = SaveFileSVG_Handler;
            ClickToolStripButton("tbImageSave", ccWin);

            KeyDownForm(frm.Name, Keys.Escape);
            frm.Dispose();
        }

        #endregion
    }
}

#endif
