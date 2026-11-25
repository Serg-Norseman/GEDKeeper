/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Events;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class EventDefEditDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblTag;
        private IComboBox _cmbTag;
        private ILabel _lblType;
        private ITextBox _txtType;
        private ICheckBox _chkEnabled;
        private ILabel _lblDesc;
        private ITextBox _txtDesc;

        [Test]
        public void Test_EventDefEditDlgController()
        {
            IEventDefEditDlg view = CreateMockView();
            var controller = new EventDefEditDlgController(view);
            controller.InitView(null);

            var eventDef = new EventDef("Birth", "sample desc", "BIRT", "sample type", true, EventTarget.etIndividual);
            controller.EventDef = eventDef;

            _txtName.Text = "sample name";
            _txtType.Text = "sample type";
            _txtDesc.Text = "sample desc";
            controller.Accept();

            Assert.AreEqual("sample name", eventDef.DisplayName);
            Assert.AreEqual("sample type", eventDef.Type);
            Assert.AreEqual("sample desc", eventDef.Description);

            controller.UpdateView();
        }

        private IEventDefEditDlg CreateMockView()
        {
            var view = Substitute.For<IEventDefEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _lblTag = SubstCtl<ILabel>(view, "lblTag");
            _cmbTag = SubstCtl<IComboBox>(view, "cmbTag");
            _lblType = SubstCtl<ILabel>(view, "lblType");
            _txtType = SubstCtl<ITextBox>(view, "txtType");
            _chkEnabled = SubstCtl<ICheckBox>(view, "chkEnabled");
            _lblDesc = SubstCtl<ILabel>(view, "lblDesc");
            _txtDesc = SubstCtl<ITextBox>(view, "txtDesc");
            return view;
        }
    }
}
