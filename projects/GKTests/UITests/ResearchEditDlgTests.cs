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
using GKTests;
using GKTests.ControlTesters;
using GKTests.Stubs;
using GKUI.Forms;
using NUnit.Extensions.Forms;
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
        private GEDCOMResearchRecord fResearchRecord;
        private IBaseWindow fBase;
        private ResearchEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowStub();
            fResearchRecord = new GEDCOMResearchRecord(fBase.Context.Tree, fBase.Context.Tree);

            fDialog = new ResearchEditDlg(fBase);
            fDialog.Research = fResearchRecord;
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
            Assert.AreEqual(fResearchRecord, fDialog.Research);

            var txtName = new TextBoxTester("txtName", fDialog);
            txtName.Enter("sample text");

            var cmbPriority = new ComboBoxTester("cmbPriority", fDialog);
            cmbPriority.Select(1);

            var cmbStatus = new ComboBoxTester("cmbStatus", fDialog);
            cmbStatus.Select(1);

            var nudPercent = new NumericUpDownTester("nudPercent", fDialog);
            nudPercent.EnterValue(11);

            // The links to other records can be added or edited only in MainWinTests
            // (where there is a complete infrastructure of the calls to BaseWin.ModifyX)

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fResearchRecord.ResearchName);
            Assert.AreEqual(GKResearchPriority.rpLow, fResearchRecord.Priority);
            Assert.AreEqual(GKResearchStatus.rsInProgress, fResearchRecord.Status);
            Assert.AreEqual(11, fResearchRecord.Percent);
            Assert.AreEqual("", fResearchRecord.StartDate.StringValue);
            Assert.AreEqual("", fResearchRecord.StopDate.StringValue);
        }

        [Test]
        public void Test_EnterDataDatesAndApply()
        {
            // Dates isn't empty
            Assert.AreEqual(fResearchRecord, fDialog.Research);

            var txtName = new TextBoxTester("txtName", fDialog);
            txtName.Enter("sample text");

            var txtStartDate = new MaskedTextBoxTester("txtStartDate", fDialog);
            txtStartDate.Enter("01.01.2000");

            var txtStopDate = new MaskedTextBoxTester("txtStopDate", fDialog);
            txtStopDate.Enter("20.02.2000");

            // The links to other records can be added or edited only in MainWinTests
            // (where there is a complete infrastructure of the calls to BaseWin.ModifyX)

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fResearchRecord.ResearchName);
            Assert.AreEqual("01 JAN 2000", fResearchRecord.StartDate.StringValue);
            Assert.AreEqual("20 FEB 2000", fResearchRecord.StopDate.StringValue);
        }
    }
}

#endif
