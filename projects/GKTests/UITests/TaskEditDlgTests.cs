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
    /// 
    /// </summary>
    [TestFixture]
    public class TaskEditDlgTests : CustomWindowTest
    {
        private GEDCOMTaskRecord fTaskRecord;
        private IBaseWindow fBase;
        private TaskEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowStub();
            fTaskRecord = new GEDCOMTaskRecord(fBase.Context.Tree);

            fDialog = new TaskEditDlg(fBase);
            fDialog.Task = fTaskRecord;
            fDialog.Show();
        }

        public override void TearDown()
        {
            fDialog.Dispose();
            fTaskRecord.Dispose();
        }

        [Test]
        public void Test_Cancel()
        {
            ClickButton("btnCancel", fDialog);
        }

        [Test]
        public void Test_EnterDataAndApply()
        {
            Assert.AreEqual(fTaskRecord, fDialog.Task);

            var txtPriority = new ComboBoxTester("txtPriority", fDialog);
            txtPriority.Select(1);

            var cmbGoalType = new ComboBoxTester("cmbGoalType", fDialog);
            for (GKGoalType gt = GKGoalType.gtIndividual; gt <= GKGoalType.gtOther; gt++) {
                cmbGoalType.Select((int)gt);
            }

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual(GKResearchPriority.rpLow, fTaskRecord.Priority);
            Assert.AreEqual("", fTaskRecord.StartDate.StringValue);
            Assert.AreEqual("", fTaskRecord.StopDate.StringValue);
        }

        [Test]
        public void Test_EnterDataDatesAndApply()
        {
            Assert.AreEqual(fTaskRecord, fDialog.Task);

            var txtPriority = new ComboBoxTester("txtPriority", fDialog);
            txtPriority.Select(1);

            var txtStartDate = new MaskedTextBoxTester("txtStartDate", fDialog);
            txtStartDate.Enter("01.01.2000");

            var txtStopDate = new MaskedTextBoxTester("txtStopDate", fDialog);
            txtStopDate.Enter("20.02.2000");

            var cmbGoalType = new ComboBoxTester("cmbGoalType", fDialog);
            for (GKGoalType gt = GKGoalType.gtIndividual; gt <= GKGoalType.gtOther; gt++) {
                cmbGoalType.Select((int)gt);
            }

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual(GKResearchPriority.rpLow, fTaskRecord.Priority);
            Assert.AreEqual("01 JAN 2000", fTaskRecord.StartDate.StringValue);
            Assert.AreEqual("20 FEB 2000", fTaskRecord.StopDate.StringValue);
        }
    }
}

#endif
