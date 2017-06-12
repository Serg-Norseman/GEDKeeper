using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

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

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            btnAddress.Size = new Size(80, 26);
            btnAddress.Text = "btnAddress";
            btnAddress.Click += btnAddress_Click;

            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageMultimedia);
            tabsData.Pages.Add(pageSources);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(620, 429);

            pageCommon.Size = new Size(612, 399);
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblEvent }
                    },
                    new TableRow {
                        Cells = { cmbEventType, txtEventName }
                    },
                    new TableRow {
                        Cells = { lblAttrValue }
                    },
                    new TableRow {
                        Cells = { txtAttribute }
                    },
                    new TableRow {
                        Cells = { lblPlace }
                    },
                    new TableRow {
                        Cells = { txtEventPlace, btnPlaceAdd, btnPlaceDelete }
                    },
                    new TableRow {
                        Cells = { lblDate }
                    },
                    new TableRow {
                        Cells = { cmbEventDateType, txtEventDate1, txtEventDate2 }
                    },
                    new TableRow {
                        Cells = { null, cmbDate1Calendar, btnBC1, cmbDate2Calendar, btnBC2 }
                    },
                    new TableRow {
                        Cells = { lblCause }
                    },
                    new TableRow {
                        Cells = { txtEventCause }
                    },
                    new TableRow {
                        Cells = { lblOrg }
                    },
                    new TableRow {
                        Cells = { txtEventOrg }
                    },
                    null
                }
            };

            btnBC2.Size = new Size(47, 21);
            btnBC2.Text = "BC";

            btnBC1.Size = new Size(47, 21);
            btnBC1.Text = "BC";

            lblEvent.Size = new Size(56, 17);
            lblEvent.Text = "lblEvent";

            lblPlace.Size = new Size(51, 17);
            lblPlace.Text = "lblPlace";

            lblDate.Size = new Size(49, 17);
            lblDate.Text = "lblDate";

            lblCause.Size = new Size(57, 17);
            lblCause.Text = "lblCause";

            lblOrg.Size = new Size(43, 17);
            lblOrg.Text = "lblOrg";

            lblAttrValue.Size = new Size(75, 17);
            lblAttrValue.Text = "lblAttrValue";

            btnPlaceAdd.Enabled = false;
            btnPlaceAdd.Size = new Size(39, 34);
            btnPlaceAdd.Click += btnPlaceAdd_Click;

            btnPlaceDelete.Enabled = false;
            btnPlaceDelete.Size = new Size(39, 34);
            btnPlaceDelete.Click += btnPlaceDelete_Click;

            cmbEventType.ReadOnly = true;
            cmbEventType.Size = new Size(259, 25);
            cmbEventType.SelectedIndexChanged += EditEventType_SelectedIndexChanged;

            txtEventName.Size = new Size(316, 24);

            txtEventPlace.Size = new Size(492, 24);
            txtEventPlace.KeyDown += EditEventPlace_KeyDown;

            cmbEventDateType.ReadOnly = true;
            cmbEventDateType.Size = new Size(168, 25);
            cmbEventDateType.SelectedIndexChanged += EditEventDateType_SelectedIndexChanged;

            txtEventDate1.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtEventDate1.AllowDrop = true;
            txtEventDate1.BackgroundColor = SystemColors.WindowBackground;
            //txtEventDate1.Mask = "00/00/0000";
            txtEventDate1.Size = new Size(196, 24);
            //txtEventDate1.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            //txtEventDate1.DragDrop += new DragEventHandler(EditEventDate1_DragDrop);
            //txtEventDate1.DragOver += new DragEventHandler(EditEventDate1_DragOver);

            txtEventDate2.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtEventDate2.AllowDrop = true;
            //txtEventDate2.Mask = "00/00/0000";
            txtEventDate2.Size = new Size(196, 24);
            //txtEventDate2.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            //txtEventDate2.DragDrop += new DragEventHandler(EditEventDate1_DragDrop);
            //txtEventDate2.DragOver += new DragEventHandler(EditEventDate1_DragOver);

            txtEventCause.Size = new Size(585, 24);

            txtEventOrg.Size = new Size(585, 24);

            txtAttribute.Size = new Size(585, 25);

            cmbDate1Calendar.ReadOnly = true;
            cmbDate1Calendar.Size = new Size(133, 25);

            cmbDate2Calendar.ReadOnly = true;
            cmbDate2Calendar.Size = new Size(133, 25);

            pageNotes.Size = new Size(612, 399);
            pageNotes.Text = "pageNotes";

            pageMultimedia.Size = new Size(612, 399);
            pageMultimedia.Text = "pageMultimedia";

            pageSources.Size = new Size(612, 399);
            pageSources.Text = "pageSources";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { btnAddress, null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(620, 492);
            Title = "EventEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
