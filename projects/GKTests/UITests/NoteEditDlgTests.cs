/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

#if !MONO

using System;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Platform;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class NoteEditDlgTests : CustomWindowTest
    {
        private GDMNoteRecord fNoteRecord;
        private IBaseWindow fBase;
        private NoteEditDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fNoteRecord = new GDMNoteRecord(fBase.Context.Tree);

            fDialog = new NoteEditDlg(fBase);
            fDialog.NoteRecord = fNoteRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fNoteRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fNoteRecord, fDialog.NoteRecord);

            EnterRichText("txtNote", fDialog, "sample text");
            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fNoteRecord.Lines.Text);
        }

        #region Handlers for external tests

        [Test]
        public void Test_Common()
        {
            EnterRichText("txtNote", fDialog, "sample text");
            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fDialog.NoteRecord.Lines.Text);
        }

        public static void NoteAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            EnterRichText("txtNote", form, "sample text");
            //Assert.AreEqual("sample text", txtNote.Text);

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
