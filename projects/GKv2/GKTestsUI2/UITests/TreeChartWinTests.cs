﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

#if !DIS_NUF

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using GDModel;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Search;
using GKTests;
using GKUI.Components;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class TreeChartWinTests : CustomWindowTest
    {

        #region Handlers for external tests

        public static void TreeChartWin_Tests(CustomWindowTest formTest, Form frm, TreeChartKind kind, string checkXRef)
        {
            Assert.IsInstanceOf(typeof(TreeChartWin), frm);

            TreeChartWin tcWin = frm as TreeChartWin;

            IBaseWindow curBase = tcWin.OwnerWindow as IBaseWindow;
            Assert.IsNotNull(curBase);

            Assert.AreEqual(kind, ((ITreeChartWin)tcWin).TreeBox.Model.Kind);
            tcWin.UpdateSettings();

            Assert.IsTrue(tcWin.AllowFilter());
            Assert.IsTrue(tcWin.AllowQuickSearch());
            Assert.IsTrue(tcWin.AllowPrint());

            // forced update
            tcWin.Refresh();

            Assert.Throws(typeof(ArgumentNullException), () => { tcWin.SelectByRec(null); });

            GDMIndividualRecord iRec = curBase.GetSelectedPerson();
            Assert.AreEqual(checkXRef, iRec.XRef);
            tcWin.SelectByRec(iRec);

            KeyDownForm(tcWin.Name, Keys.F5);
            KeyDownForm(tcWin.Name, Keys.F6);
            KeyDownForm(tcWin.Name, Keys.F7);

            //KeyDownForm(tcWin.Name, Keys.F | Keys.Control);

            tcWin.NavPrev();
            tcWin.NavNext();

            formTest.ModalFormHandler = TreeFilterDlgTests.TreeFilterDlg_btnAccept_Handler;
            tcWin.SetFilter();

            IList<ISearchResult> search = tcWin.FindAll("Maria");
            Assert.AreEqual(1, search.Count);

            ClickToolStripMenuItem("miModeBoth", tcWin);
            ClickToolStripMenuItem("miModeAncestors", tcWin);
            ClickToolStripMenuItem("miModeDescendants", tcWin);

            ClickToolStripMenuItem("miCertaintyIndex", tcWin);
            ClickToolStripMenuItem("miTraceKinships", tcWin);
            ClickToolStripMenuItem("miTraceSelected", tcWin);

            var ctl = new ControlTester("fTreeBox", frm);
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Add));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Subtract));
            ctl.FireEvent("KeyDown", new KeyEventArgs(Keys.Back));
            ctl.FireEvent("DoubleClick", new EventArgs());

            formTest.Mouse.UseOn(ctl);

            try {
                int sx = 10 + 20;
                int sy = ctl.Properties.Height / 2;
                formTest.Mouse.Hover(sx, sy);
                formTest.Mouse.Press(MouseButtons.Left);
                formTest.Mouse.Hover(sx, sy + 20); // generations control
                formTest.Mouse.Release(MouseButtons.Left);

                sx = ctl.Properties.Width - 10 - 30;
                formTest.Mouse.Hover(sx, sy);
                formTest.Mouse.Press(MouseButtons.Left);
                formTest.Mouse.Hover(sx, sy + 20); // scale control
                formTest.Mouse.Release(MouseButtons.Left);
            } catch {
                // strange bug when testing from VS (Mouse.Press())
            }

            //

            var tbox = ctl.Properties as TreeChartBox;
            Assert.IsNotNull(tbox);

            // handlers tests
            //ClickToolStripMenuItem("miEdit", tcWin);
            //ClickToolStripMenuItem("miFatherAdd", tcWin);
            //ClickToolStripMenuItem("miMotherAdd", tcWin);
            //ClickToolStripMenuItem("miSpouseAdd", tcWin);
            //ClickToolStripMenuItem("miSonAdd", tcWin);
            //ClickToolStripMenuItem("miDaughterAdd", tcWin);
            //ClickToolStripMenuItem("miFamilyAdd", tcWin);
            //ClickToolStripMenuItem("miDelete", tcWin);
            //ClickToolStripMenuItem("miRebuildKinships", tcWin);
            //ClickToolStripMenuItem("miFillColor", tcWin);
            //ClickToolStripMenuItem("miFillImage", tcWin);
            //ClickToolStripMenuItem("miRebuildTree", tcWin);

            try {
                formTest.ModalFormHandler = SaveFileJPG_Handler;
                ClickToolStripButton("tbImageSave", tcWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.jpg"));
            }

            // FIXME exception!
            //ModalFormHandler = SaveSnapshotEMF_Handler;
            //ClickToolStripButton("tbImageSave", tcWin);

            try {
                formTest.ModalFormHandler = SaveFileSVG_Handler;
                ClickToolStripButton("tbImageSave", tcWin);
            } finally {
                TestUtils.RemoveTestFile(TestUtils.GetTempFilePath("test.svg"));
            }

#if !CI_MODE
            //formTest.ModalFormHandler = PrintDialog_Handler;
            //ClickToolStripButton("tbDocPrint", tcWin);

            formTest.ModalFormHandler = PrintDialog_Handler;
            ClickToolStripButton("tbDocPreview", tcWin);
#endif

            KeyDownForm(frm.Name, Keys.Escape);
            frm.Dispose();
        }

        #endregion
    }
}

#endif
