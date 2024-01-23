/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

#if !MONO && !DIS_NUF

using System;
using System.Threading;
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKTests;
using GKTests.Stubs;
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

        public static string MediaSampleFile;

        public override void Setup()
        {
            TestUtilsUI.InitUITest();
            LangMan.DefInit();

            GlobalOptions.Instance.AllowMediaStoreReferences = true;

            fBase = new BaseWindowStub();
            fMultimediaRecord = new GDMMultimediaRecord(fBase.Context.Tree);
            fMultimediaRecord.FileReferences.Add(new GDMFileReferenceWithTitle());

            fDialog = new MediaEditDlg(fBase);
            fDialog.MultimediaRecord = fMultimediaRecord;
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

        [Test, RequiresThread(ApartmentState.STA)]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fMultimediaRecord, fDialog.MultimediaRecord);

            fFormTest = this;
            try {
                MultimediaRecord_Add_Handler(null, IntPtr.Zero, fDialog);
            } finally {
                TestUtils.RemoveTestFile(MediaEditDlgTests.MediaSampleFile);
            }

            Assert.AreEqual("sample text", fMultimediaRecord.GetFileTitle());
        }

        #region Handlers for external tests

        public static void MultimediaRecord_Add_Handler(string name, IntPtr ptr, Form form)
        {
            EnterText("txtName", form, "sample text");
            SelectCombo("cmbMediaType", form, 1);
            SelectCombo("cmbStoreType", form, 0); // Reference

            MediaSampleFile = TestUtils.PrepareTestFile("shaytan_plant.jpg");
            SetOpenedFile(fFormTest, MediaSampleFile);
            ClickButton("btnFileSelect", form);

            ClickButton("btnAccept", form);
        }

        #endregion
    }
}

#endif
