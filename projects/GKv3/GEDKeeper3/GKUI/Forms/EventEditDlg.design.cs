using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
        private TextBox txtEventCause;
        private TextBox txtEventOrg;
        private ComboBox txtAttribute;
        private Button btnPlaceAdd;
        private Button btnPlaceDelete;
        private GKDateControl dateCtl;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblEvent = new Label();
            lblEvent.Text = "lblEvent";

            lblPlace = new Label();
            lblPlace.Text = "lblPlace";

            lblDate = new Label();
            lblDate.Text = "lblDate";

            lblCause = new Label();
            lblCause.Text = "lblCause";

            lblOrg = new Label();
            lblOrg.Text = "lblOrg";

            lblAttrValue = new Label();
            lblAttrValue.Text = "lblAttrValue";

            btnPlaceAdd = new Button();
            btnPlaceAdd.Enabled = false;
            btnPlaceAdd.Size = UIHelper.ShortButtonSize;
            btnPlaceAdd.Click += btnPlaceAdd_Click;

            btnPlaceDelete = new Button();
            btnPlaceDelete.Enabled = false;
            btnPlaceDelete.Size = UIHelper.ShortButtonSize;
            btnPlaceDelete.Click += btnPlaceDelete_Click;

            cmbEventType = new ComboBox();
            cmbEventType.ReadOnly = true;
            cmbEventType.SelectedIndexChanged += EditEventType_SelectedIndexChanged;

            txtEventName = new TextBox();

            txtEventPlace = new TextBox();
            txtEventPlace.KeyDown += EditEventPlace_KeyDown;

            txtEventCause = new TextBox();

            txtEventOrg = new TextBox();

            txtAttribute = new ComboBox();

            dateCtl = new GKDateControl();

            //

            pageCommon = new TabPage();
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblEvent, TableLayout.Horizontal(10, cmbEventType, txtEventName) }
                    },
                    new TableRow {
                        Cells = { lblAttrValue, txtAttribute }
                    },
                    new TableRow {
                        Cells = { lblPlace, TableLayout.Horizontal(10, new TableCell(txtEventPlace, true), btnPlaceAdd, btnPlaceDelete) }
                    },
                    new TableRow {
                        Cells = { lblDate, dateCtl }
                    },
                    new TableRow {
                        Cells = { lblCause, txtEventCause }
                    },
                    new TableRow {
                        Cells = { lblOrg, txtEventOrg }
                    }
                }
            };

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            pageMultimedia = new TabPage();
            pageMultimedia.Text = "pageMultimedia";

            pageSources = new TabPage();
            pageSources.Text = "pageSources";

            tabsData = new TabControl();
            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageNotes);
            tabsData.Pages.Add(pageMultimedia);
            tabsData.Pages.Add(pageSources);

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = UIHelper.LongButtonSize;
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = UIHelper.LongButtonSize;
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            btnAddress = new Button();
            btnAddress.Size = UIHelper.LongButtonSize;
            btnAddress.Text = "btnAddress";
            btnAddress.Click += btnAddress_Click;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(btnAddress, null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "EventEditDlg";

            SetPredefProperties(496, 394);
            ResumeLayout();
        }
    }
}
