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
    public class MediaEditDlgTests : CustomWindowTest
    {
        private GDMMultimediaRecord fMultimediaRecord;
        private IBaseWindow fBase;
        private MediaEditDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fMultimediaRecord = new GDMMultimediaRecord(fBase.Context.Tree);
            fMultimediaRecord.FileReferences.Add(new GDMFileReferenceWithTitle());

            fDialog = new MediaEditDlg(fBase);
            fDialog.MediaRec = fMultimediaRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fMultimediaRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fMultimediaRecord, fDialog.MediaRec);

            var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);

            //ModalFormHandler = OpenFile_Cancel_Handler;
            //ClickButton("btnFileSelect", fDialog);

            ClickButton("btnAccept", fDialog);

            //Assert.AreEqual("sample text", fMultimediaRecord.GetFileTitle());
        }

        #region Handlers for external tests

        public static void MediaAdd_Mini_Handler(string name, IntPtr ptr, Form form)
        {
            //EnterText("txtName", "sample text");

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
