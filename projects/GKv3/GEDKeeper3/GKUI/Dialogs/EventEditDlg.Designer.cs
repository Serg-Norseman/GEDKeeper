using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class EventEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageSources;
        private Button btnAddress;
        private TabPage pageCommon;
        private Label lblEvent;
        private Label lblPlace;
        private Label lblDate;
        private Label lblCause;
        private Label lblOrg;
        private Label lblAttrValue;
        private ComboBox cmbEventType;
        private TextBox txtEventName;
        private TextBox txtEventPlace;
        private ComboBox cmbEventDateType;
        private MaskedTextBox txtEventDate1;
        private MaskedTextBox txtEventDate2;
        private TextBox txtEventCause;
        private TextBox txtEventOrg;
        private ComboBox txtAttribute;
        private Button btnPlaceAdd;
        private Button btnPlaceDelete;
        private ComboBox cmbDate1Calendar;
        private ComboBox cmbDate2Calendar;
        private CheckBox btnBC1;
        private CheckBox btnBC2;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageCommon = new TabPage();
            btnBC2 = new CheckBox();
            btnBC1 = new CheckBox();
            lblEvent = new Label();
            lblPlace = new Label();
            lblDate = new Label();
            lblCause = new Label();
            lblOrg = new Label();
            lblAttrValue = new Label();
            btnPlaceAdd = new Button();
            btnPlaceDelete = new Button();
            cmbEventType = new ComboBox();
            txtEventName = new TextBox();
            txtEventPlace = new TextBox();
            cmbEventDateType = new ComboBox();
            txtEventDate1 = new MaskedTextBox();
            txtEventDate2 = new MaskedTextBox();
            txtEventCause = new TextBox();
            txtEventOrg = new TextBox();
            txtAttribute = new ComboBox();
            cmbDate1Calendar = new ComboBox();
            cmbDate2Calendar = new ComboBox();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            pageSources = new TabPage();
            btnAddress = new Button();
            tabsData.SuspendLayout();
            pageCommon.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(371, 447);
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(494, 447);
            btnCancel.Size = new Size(114, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Controls.Add(pageCommon);
            tabsData.Controls.Add(pageNotes);
            tabsData.Controls.Add(pageMultimedia);
            tabsData.Controls.Add(pageSources);
            tabsData.Dock = DockStyle.Top;
            tabsData.Location = new Point(0, 0);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(620, 429);

            pageCommon.Controls.Add(btnBC2);
            pageCommon.Controls.Add(btnBC1);
            pageCommon.Controls.Add(lblEvent);
            pageCommon.Controls.Add(lblPlace);
            pageCommon.Controls.Add(lblDate);
            pageCommon.Controls.Add(lblCause);
            pageCommon.Controls.Add(lblOrg);
            pageCommon.Controls.Add(lblAttrValue);
            pageCommon.Controls.Add(btnPlaceAdd);
            pageCommon.Controls.Add(btnPlaceDelete);
            pageCommon.Controls.Add(cmbEventType);
            pageCommon.Controls.Add(txtEventName);
            pageCommon.Controls.Add(txtEventPlace);
            pageCommon.Controls.Add(cmbEventDateType);
            pageCommon.Controls.Add(txtEventDate1);
            pageCommon.Controls.Add(txtEventDate2);
            pageCommon.Controls.Add(txtEventCause);
            pageCommon.Controls.Add(txtEventOrg);
            pageCommon.Controls.Add(txtAttribute);
            pageCommon.Controls.Add(cmbDate1Calendar);
            pageCommon.Controls.Add(cmbDate2Calendar);
            pageCommon.Location = new Point(4, 26);
            pageCommon.Size = new Size(612, 399);
            pageCommon.Text = "pageCommon";

            btnBC2.Location = new Point(542, 240);
            btnBC2.Size = new Size(47, 21);
            btnBC2.Text = "BC";

            btnBC1.Location = new Point(332, 242);
            btnBC1.Size = new Size(47, 21);
            btnBC1.Text = "BC";

            lblEvent.Location = new Point(11, 10);
            lblEvent.Size = new Size(56, 17);
            lblEvent.Text = "lblEvent";

            lblPlace.Location = new Point(11, 126);
            lblPlace.Size = new Size(51, 17);
            lblPlace.Text = "lblPlace";

            lblDate.Location = new Point(11, 185);
            lblDate.Size = new Size(49, 17);
            lblDate.Text = "lblDate";

            lblCause.Location = new Point(11, 282);
            lblCause.Size = new Size(57, 17);
            lblCause.Text = "lblCause";

            lblOrg.Location = new Point(11, 340);
            lblOrg.Size = new Size(43, 17);
            lblOrg.Text = "lblOrg";

            lblAttrValue.Location = new Point(11, 68);
            lblAttrValue.Size = new Size(75, 17);
            lblAttrValue.Text = "lblAttrValue";

            btnPlaceAdd.Enabled = false;
            btnPlaceAdd.Location = new Point(511, 142);
            btnPlaceAdd.Size = new Size(39, 34);
            btnPlaceAdd.Click += btnPlaceAdd_Click;

            btnPlaceDelete.Enabled = false;
            btnPlaceDelete.Location = new Point(557, 142);
            btnPlaceDelete.Size = new Size(39, 34);
            btnPlaceDelete.Click += btnPlaceDelete_Click;

            cmbEventType.ReadOnly = true;
            cmbEventType.Location = new Point(11, 29);
            cmbEventType.Size = new Size(259, 25);
            cmbEventType.SelectedIndexChanged += EditEventType_SelectedIndexChanged;

            txtEventName.Location = new Point(280, 29);
            txtEventName.Size = new Size(316, 24);

            txtEventPlace.Location = new Point(11, 146);
            txtEventPlace.Size = new Size(492, 24);
            txtEventPlace.KeyDown += EditEventPlace_KeyDown;

            cmbEventDateType.ReadOnly = true;
            cmbEventDateType.Location = new Point(11, 204);
            cmbEventDateType.Size = new Size(168, 25);
            cmbEventDateType.SelectedIndexChanged += EditEventDateType_SelectedIndexChanged;

            //txtEventDate1.AllowDrop = true;
            txtEventDate1.BackgroundColor = SystemColors.WindowBackground;
            txtEventDate1.Location = new Point(190, 204);
            txtEventDate1.Mask = "00/00/0000";
            txtEventDate1.Size = new Size(196, 24);
            txtEventDate1.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            //txtEventDate1.DragDrop += new DragEventHandler(EditEventDate1_DragDrop);
            //txtEventDate1.DragOver += new DragEventHandler(EditEventDate1_DragOver);

            //txtEventDate2.AllowDrop = true;
            txtEventDate2.Location = new Point(400, 204);
            txtEventDate2.Mask = "00/00/0000";
            txtEventDate2.Size = new Size(196, 24);
            txtEventDate2.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            //txtEventDate2.DragDrop += new DragEventHandler(EditEventDate1_DragDrop);
            //txtEventDate2.DragOver += new DragEventHandler(EditEventDate1_DragOver);

            txtEventCause.Location = new Point(11, 301);
            txtEventCause.Size = new Size(585, 24);

            txtEventOrg.Location = new Point(11, 359);
            txtEventOrg.Size = new Size(585, 24);

            txtAttribute.Location = new Point(11, 87);
            txtAttribute.Size = new Size(585, 25);

            cmbDate1Calendar.ReadOnly = true;
            cmbDate1Calendar.Location = new Point(190, 243);
            cmbDate1Calendar.Size = new Size(133, 25);

            cmbDate2Calendar.ReadOnly = true;
            cmbDate2Calendar.Location = new Point(400, 242);
            cmbDate2Calendar.Size = new Size(133, 25);

            pageNotes.Location = new Point(4, 26);
            pageNotes.Size = new Size(612, 399);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Location = new Point(4, 26);
            pageMultimedia.Size = new Size(612, 399);
            pageMultimedia.Text = "pageMultimedia";

            pageSources.Location = new Point(4, 26);
            pageSources.Size = new Size(612, 399);
            pageSources.Text = "pageSources";

            btnAddress.Location = new Point(11, 447);
            btnAddress.Size = new Size(114, 30);
            btnAddress.Text = "btnAddress";
            btnAddress.Click += btnAddress_Click;

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(620, 492);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(tabsData);
            Controls.Add(btnAddress);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "EventEditDlg";
            tabsData.ResumeLayout();
            pageCommon.ResumeLayout();
            ResumeLayout();
        }
    }
}
