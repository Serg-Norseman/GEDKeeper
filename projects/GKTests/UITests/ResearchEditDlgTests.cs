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

#if !MONO

using GDModel;
using GKCore.Interfaces;
using GKTests;
using GKTests.Stubs;
using GKUI.Platform;
using NUnit.Framework;

namespace GKUI.Forms
{
    /// <summary>
    /// Isolated test of dialogue (ResearchEditDlg), without the ability
    /// to add or change references to other records.
    /// </summary>
    [TestFixture]
    public class ResearchEditDlgTests : CustomWindowTest
    {
        private GDMResearchRecord fResearchRecord;
        private IBaseWindow fBase;
        private ResearchEditDlg fDialog;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
            fResearchRecord = new GDMResearchRecord(fBase.Context.Tree);

            fDialog = new ResearchEditDlg(fBase);
            fDialog.ResearchRecord = fResearchRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fResearchRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            // Empty dates
            Assert.AreEqual(fResearchRecord, fDialog.ResearchRecord);

            EnterText("txtName", fDialog, "sample text");
            SelectCombo("cmbPriority", fDialog, 1);
            SelectCombo("cmbStatus", fDialog, 1);
            EnterNumeric("nudPercent", fDialog, 11);

            // The links to other records can be added or edited only in MainWinTests
            // (where there is a complete infrastructure of the calls to BaseWin.ModifyX)

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fResearchRecord.ResearchName);
            Assert.AreEqual(GDMResearchPriority.rpLow, fResearchRecord.Priority);
            Assert.AreEqual(GDMResearchStatus.rsInProgress, fResearchRecord.Status);
            Assert.AreEqual(11, fResearchRecord.Percent);
            Assert.AreEqual("", fResearchRecord.StartDate.StringValue);
            Assert.AreEqual("", fResearchRecord.StopDate.StringValue);
        }

        [Test]
        public void Test_EnterDataDatesAndApply()
        {
            // Dates isn't empty
            Assert.AreEqual(fResearchRecord, fDialog.ResearchRecord);

            EnterText("txtName", fDialog, "sample text");
            EnterMaskedText("txtStartDate", fDialog, "01.01.2000");
            EnterMaskedText("txtStopDate", fDialog, "02.02.2000");

            // The links to other records can be added or edited only in MainWinTests
            // (where there is a complete infrastructure of the calls to BaseWin.ModifyX)

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fResearchRecord.ResearchName);
            Assert.AreEqual("01 JAN 2000", fResearchRecord.StartDate.StringValue);
            Assert.AreEqual("02 FEB 2000", fResearchRecord.StopDate.StringValue);
        }
    }
}

#endif
