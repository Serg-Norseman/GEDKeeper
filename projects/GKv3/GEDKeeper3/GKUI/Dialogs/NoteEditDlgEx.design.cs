using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class NoteEditDlgEx
    {
        private Button btnAccept;
        private Button btnCancel;
        private TextArea txtNote;
        private GKUI.Components.HyperView hyperView1;
        private ButtonToolItem cmbSizes; // FIXME: GKv3 DevRestriction
        private ContextMenu menuSizes;
        private ButtonMenuItem miClear;
        private ButtonMenuItem miExport;
        private ButtonMenuItem miImport;
        private ButtonMenuItem miSelectAndCopy;
        private ButtonToolItem ddbtnActions; // FIXME: GKv3 DevRestriction
        private ContextMenu menuActions;
        private SeparatorToolItem SeparatorToolItem1;
        private ButtonToolItem btnURL;
        private ButtonToolItem btnUnderline;
        private ButtonToolItem btnItalic;
        private ButtonToolItem btnBold;
        private TabPage pagePreview;
        private ToolBar toolStrip1;
        private TabPage pageEditor;
        private TabControl tabControl1;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabControl1 = new TabControl();
            pageEditor = new TabPage();
            txtNote = new TextArea();
            toolStrip1 = new ToolBar();
            btnBold = new ButtonToolItem();
            btnItalic = new ButtonToolItem();
            btnUnderline = new ButtonToolItem();
            btnURL = new ButtonToolItem();
            cmbSizes = new ButtonToolItem();
            SeparatorToolItem1 = new SeparatorToolItem();
            ddbtnActions = new ButtonToolItem();
            miSelectAndCopy = new ButtonMenuItem();
            miImport = new ButtonMenuItem();
            miExport = new ButtonMenuItem();
            miClear = new ButtonMenuItem();
            pagePreview = new TabPage();
            hyperView1 = new GKUI.Components.HyperView();
            menuSizes = new ContextMenu();
            menuActions = new ContextMenu();

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(101, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(101, 31);
            btnCancel.Text = "btnCancel";

            tabControl1.Pages.Add(pageEditor);
            tabControl1.Pages.Add(pagePreview);
            tabControl1.SelectedIndex = 0;
            tabControl1.Size = new Size(777, 383);
            tabControl1.SelectedIndexChanged += tabControl1_SelectedIndexChanged;

            pageEditor.BackgroundColor = SystemColors.Control;
            pageEditor.Content = txtNote;
            pageEditor.Size = new Size(769, 353);
            pageEditor.Text = "pageEditor";

            txtNote.AcceptsReturn = true;
            txtNote.Size = new Size(763, 319);

            toolStrip1.Items.AddRange(new ToolItem[] {
                                          btnBold,
                                          btnItalic,
                                          btnUnderline,
                                          btnURL,
                                          cmbSizes,
                                          SeparatorToolItem1,
                                          ddbtnActions});

            //btnBold.Font = new Font("Tahoma", 9F, FontStyle.Bold);
            btnBold.Text = "B";
            btnBold.Click += btnBold_Click;

            //btnItalic.Font = new Font("Tahoma", 9F, FontStyle.Italic);
            btnItalic.Text = "I";
            btnItalic.Click += btnItalic_Click;

            //btnUnderline.Font = new Font("Tahoma", 9F, FontStyle.None, FontDecoration.Underline);
            btnUnderline.Text = "U";
            btnUnderline.Click += btnUnderline_Click;

            //btnURL.Font = new Font("Tahoma", 9F, FontStyle.None, FontDecoration.Underline);
            //btnURL.TextColor = Colors.Blue;
            btnURL.Text = "URL";
            btnURL.Click += btnURL_Click;

            cmbSizes.Text = "cmbSizes";
            //cmbSizes.SelectedIndexChanged += cmbSizes_SelectedIndexChanged;
            cmbSizes.Click += (sender, e) => menuSizes.Show(this);

            menuActions.Items.AddRange(new MenuItem[] {
                                            miSelectAndCopy,
                                            miImport,
                                            miExport,
                                            miClear});
            ddbtnActions.Text = "Actions";
            ddbtnActions.Click += (sender, e) => menuActions.Show(this);

            miSelectAndCopy.Text = "miSelectAndCopy";
            miSelectAndCopy.Click += miSelectAndCopy_Click;

            miImport.Text = "miImport";
            miImport.Click += miImport_Click;

            miExport.Text = "miExport";
            miExport.Click += miExport_Click;

            miClear.Text = "miClear";
            miClear.Click += miClear_Click;

            pagePreview.BackgroundColor = SystemColors.Control;
            pagePreview.Content = hyperView1;
            pagePreview.Size = new Size(769, 353);
            pagePreview.Text = "pagePreview";

            //hyperView1.AutoScroll = true;
            //hyperView1.AutoScrollMinSize = new Size(4, 0);
            //hyperView1.BorderStyle = BorderStyle.Fixed3D;
            hyperView1.BorderWidth = 0;
            hyperView1.LinkColor = Colors.Blue;
            hyperView1.Size = new Size(763, 347);

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabControl1 }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(797, 457);
            Title = "NoteEditDlg";
            ToolBar = toolStrip1;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
