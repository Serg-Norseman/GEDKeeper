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
using GKCore.Types;
using GKTests;
using GKTests.Mocks;
using GKUI.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class RecordSelectDlgTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private RecordSelectDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowMock();

            fDialog = new RecordSelectDlg();
            fDialog.InitDialog(fBase);
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
        }

        [Test]
        public void Test_Common()
        {
            Assert.AreEqual("*", fDialog.Filter);
            Assert.AreEqual(GEDCOMRecordType.rtNone, fDialog.RecType);
            Assert.AreEqual(TargetMode.tmNone, fDialog.TargetMode);
            Assert.AreEqual(null, fDialog.ResultRecord);

            ClickButton("btnCancel", fDialog);
        }

        #region Handlers for external tests
        #endregion
    }
}

#endif
