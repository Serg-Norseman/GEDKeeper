/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKTests.Mocks;
using GKUI.Dialogs;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKTests.UITests
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class MediaEditDlgTests : CustomWindowTest
    {
        private GEDCOMMultimediaRecord fMultimediaRecord;
        private IBaseWindow fBase;
        private MediaEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();
            fMultimediaRecord = new GEDCOMMultimediaRecord(fBase.Context.Tree, fBase.Context.Tree, "", "");
            fMultimediaRecord.FileReferences.Add(new GEDCOMFileReferenceWithTitle(fBase.Context.Tree, fMultimediaRecord, "", ""));

            fDialog = new MediaEditDlg();
            fDialog.InitDialog(fBase);
            fDialog.MediaRec = fMultimediaRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fBase, fDialog.Base);
            Assert.AreEqual(fMultimediaRecord, fDialog.MediaRec);

            var txtName = new TextBoxTester("txtName");
            txtName.Enter("sample text");
            Assert.AreEqual("sample text", txtName.Text);

            ClickButton("btnAccept", fDialog);

            //Assert.AreEqual("sample text", fMultimediaRecord.GetFileTitle());
        }
    }
}

#endif
