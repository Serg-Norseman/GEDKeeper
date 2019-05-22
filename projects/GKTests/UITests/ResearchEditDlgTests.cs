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
        private GDMResearchRecord fResearchRecord;
        private IBaseWindow fBase;
        private ResearchEditDlg fDialog;

        public override void Setup()
        {
            base.Setup();

            fBase = new BaseWindowStub();
            fResearchRecord = new GDMResearchRecord(fBase.Context.Tree);

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
            Assert.AreEqual(fResearchRecord, fDialog.Research);

            EnterText("txtName", fDialog, "sample text");
            EnterMaskedText("txtStartDate", fDialog, "01.01.2000");
            EnterMaskedText("txtStopDate", fDialog, "20.02.2000");

            // The links to other records can be added or edited only in MainWinTests
            // (where there is a complete infrastructure of the calls to BaseWin.ModifyX)

            ClickButton("btnAccept", fDialog);

            Assert.AreEqual("sample text", fResearchRecord.ResearchName);
            Assert.AreEqual("01 JAN 2000", fResearchRecord.StartDate.StringValue);
            Assert.AreEqual("20 FEB 2000", fResearchRecord.StopDate.StringValue);
        }

        #region Handlers for external tests

        public static void ResearchEditDlg_Handler(ResearchEditDlg dlg)
        {
            GDMResearchRecord resRecord = dlg.Research;

            // tasks
            SelectTab("tabsData", dlg, 0);
            Assert.AreEqual(0, resRecord.Tasks.Count);
            RecordSelectDlgTests.SetCreateItemHandler(fFormTest, TaskEditDlgTests.TaskAdd_Mini_Handler);
            ClickToolStripButton("fTasksList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, resRecord.Tasks.Count);

            SelectSheetListItem("fTasksList", dlg, 0);
            SetModalFormHandler(fFormTest, TaskEditDlgTests.TaskAdd_Mini_Handler);
            ClickToolStripButton("fTasksList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, resRecord.Tasks.Count);

            SelectSheetListItem("fTasksList", dlg, 0);
            SetModalFormHandler(fFormTest, MessageBox_YesHandler);
            ClickToolStripButton("fTasksList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, resRecord.Tasks.Count);

            // communications
            SelectTab("tabsData", dlg, 1);
            Assert.AreEqual(0, resRecord.Communications.Count);
            RecordSelectDlgTests.SetCreateItemHandler(fFormTest, CommunicationEditDlgTests.CommunicationAdd_Mini_Handler);
            ClickToolStripButton("fCommunicationsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, resRecord.Communications.Count);

            SelectSheetListItem("fCommunicationsList", dlg, 0);
            SetModalFormHandler(fFormTest, CommunicationEditDlgTests.CommunicationAdd_Mini_Handler);
            ClickToolStripButton("fCommunicationsList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, resRecord.Communications.Count);

            SelectSheetListItem("fCommunicationsList", dlg, 0);
            SetModalFormHandler(fFormTest, MessageBox_YesHandler);
            ClickToolStripButton("fCommunicationsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, resRecord.Communications.Count);

            // groups
            SelectTab("tabsData", dlg, 2);
            Assert.AreEqual(0, resRecord.Groups.Count);
            RecordSelectDlgTests.SetCreateItemHandler(fFormTest, GroupEditDlgTests.GroupAdd_Mini_Handler);
            ClickToolStripButton("fGroupsList_ToolBar_btnAdd", dlg);
            Assert.AreEqual(1, resRecord.Groups.Count);
            Assert.AreEqual("sample group", ((GDMGroupRecord)resRecord.Groups[0].Value).GroupName);

            SelectSheetListItem("fGroupsList", dlg, 0);
            SetModalFormHandler(fFormTest, GroupEditDlgTests.GroupAdd_Mini_Handler);
            ClickToolStripButton("fGroupsList_ToolBar_btnEdit", dlg);
            Assert.AreEqual(1, resRecord.Groups.Count);

            SelectSheetListItem("fGroupsList", dlg, 0);
            SetModalFormHandler(fFormTest, MessageBox_YesHandler);
            ClickToolStripButton("fGroupsList_ToolBar_btnDelete", dlg);
            Assert.AreEqual(0, resRecord.Groups.Count);

            ClickButton("btnAccept", dlg);
        }

        #endregion
    }
}

#endif
