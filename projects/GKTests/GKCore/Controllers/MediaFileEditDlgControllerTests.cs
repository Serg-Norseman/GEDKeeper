/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class MediaFileEditDlgControllerTests : ControllerTest
    {
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnView;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblType;
        private IComboBox _cmbMediaType;
        private ILabel _lblStoreType;
        private IComboBox _cmbStoreType;
        private ILabel _lblFile;
        private ITextBox _txtFile;
        private IButton _btnFileSelect;

        [Test]
        public void Test_MediaFileEditDlgController()
        {
            IMediaFileEditDlg view = CreateMockView();
            var controller = new MediaFileEditDlgController(view);
            controller.InitView(fBaseWin);

            var fileRef = new GDMFileReferenceWithTitle();
            fileRef.StringValue = "http://sample.url";
            controller.FileRef = fileRef;

            _txtName.Text = "sample title";
            _cmbMediaType.SelectedIndex = 1;
            controller.Accept();

            Assert.AreEqual("sample title", fileRef.Title);
            Assert.AreEqual(GDMMediaType.mtAudio, fileRef.MediaType);
        }

        private IMediaFileEditDlg CreateMockView()
        {
            var view = Substitute.For<IMediaFileEditDlg>();
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _btnView = SubstCtl<IButton>(view, "btnView");
            _lblName = SubstCtl<ILabel>(view, "lblName");
            _txtName = SubstCtl<ITextBox>(view, "txtName");
            _lblType = SubstCtl<ILabel>(view, "lblType");
            _cmbMediaType = SubstCtl<IComboBox>(view, "cmbMediaType");
            _lblStoreType = SubstCtl<ILabel>(view, "lblStoreType");
            _cmbStoreType = SubstCtl<IComboBox>(view, "cmbStoreType");
            _lblFile = SubstCtl<ILabel>(view, "lblFile");
            _txtFile = SubstCtl<ITextBox>(view, "txtFile");
            _btnFileSelect = SubstCtl<IButton>(view, "btnFileSelect");
            return view;
        }
    }
}
