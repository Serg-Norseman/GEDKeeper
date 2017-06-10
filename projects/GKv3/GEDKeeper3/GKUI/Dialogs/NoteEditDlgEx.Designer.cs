using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class NoteEditDlgEx
    {
        private Button btnAccept;
        private Button btnCancel;
        private TextArea txtNote;
        private GKUI.Components.HyperView hyperView1;
        //private ToolStripComboBox cmbSizes; // FIXME: GKv3 DevRestriction
        private ButtonMenuItem miClear;
        private ButtonMenuItem miExport;
        private ButtonMenuItem miImport;
        private ButtonMenuItem miSelectAndCopy;
        //private ToolStripDropDownButton ddbtnActions; // FIXME: GKv3 DevRestriction
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
            cmbSizes = new ToolStripComboBox();
            SeparatorToolItem1 = new SeparatorToolItem();
            ddbtnActions = new ToolStripDropDownButton();
            miSelectAndCopy = new ButtonMenuItem();
            miImport = new ButtonMenuItem();
            miExport = new ButtonMenuItem();
            miClear = new ButtonMenuItem();
            pagePreview = new TabPage();
            hyperView1 = new GKUI.Components.HyperView();
            tabControl1.SuspendLayout();
            pageEditor.SuspendLayout();
            pagePreview.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(565, 406);
            btnAccept.Size = new Size(101, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(686, 406);
            btnCancel.Size = new Size(101, 31);
            btnCancel.Text = "btnCancel";

            tabControl1.Controls.Add(pageEditor);
            tabControl1.Controls.Add(pagePreview);
            tabControl1.Dock = DockStyle.Top;
            tabControl1.Location = new Point(10, 10);
            tabControl1.SelectedIndex = 0;
            tabControl1.Size = new Size(777, 383);
            tabControl1.SelectedIndexChanged += tabControl1_SelectedIndexChanged;

            pageEditor.BackgroundColor = SystemColors.Control;
            pageEditor.Controls.Add(txtNote);
            pageEditor.Controls.Add(toolStrip1);
            pageEditor.Location = new Point(4, 26);
            pageEditor.Padding = new Padding(3);
            pageEditor.Size = new Size(769, 353);
            pageEditor.Text = "pageEditor";

            txtNote.AcceptsReturn = true;
            txtNote.Dock = DockStyle.Fill;
            txtNote.Location = new Point(3, 31);
            txtNote.Size = new Size(763, 319);

            toolStrip1.Font = new Font("Tahoma", 9F, FontStyle.None);
            toolStrip1.Items.AddRange(new ToolItem[] {
                                          btnBold,
                                          btnItalic,
                                          btnUnderline,
                                          btnURL,
                                          cmbSizes,
                                          SeparatorToolItem1,
                                          ddbtnActions});
            toolStrip1.Location = new Point(3, 3);
            toolStrip1.Size = new Size(763, 28);
            toolStrip1.Text = "toolStrip1";

            btnBold.Font = new Font("Tahoma", 9F, FontStyle.Bold);
            btnBold.Text = "B";
            btnBold.Click += btnBold_Click;

            btnItalic.Font = new Font("Tahoma", 9F, FontStyle.Italic);
            btnItalic.Text = "I";
            btnItalic.Click += btnItalic_Click;

            btnUnderline.Font = new Font("Tahoma", 9F, FontStyle.None, FontDecoration.Underline);
            btnUnderline.Text = "U";
            btnUnderline.Click += btnUnderline_Click;

            btnURL.Font = new Font("Tahoma", 9F, FontStyle.None, FontDecoration.Underline);
            btnURL.TextColor = Colors.Blue;
            btnURL.Text = "URL";
            btnURL.Click += btnURL_Click;

            cmbSizes.Name = "cmbSizes";
            cmbSizes.Size = new Size(121, 28);
            cmbSizes.SelectedIndexChanged += cmbSizes_SelectedIndexChanged;

            ddbtnActions.Items.AddRange(new ToolItem[] {
                                            miSelectAndCopy,
                                            miImport,
                                            miExport,
                                            miClear});
            ddbtnActions.Name = "ddbtnActions";
            ddbtnActions.Size = new Size(67, 25);
            ddbtnActions.Text = "Actions";

            miSelectAndCopy.Text = "miSelectAndCopy";
            miSelectAndCopy.Click += miSelectAndCopy_Click;

            miImport.Text = "miImport";
            miImport.Click += miImport_Click;

            miExport.Text = "miExport";
            miExport.Click += miExport_Click;

            miClear.Text = "miClear";
            miClear.Click += miClear_Click;

            pagePreview.BackgroundColor = SystemColors.Control;
            pagePreview.Controls.Add(hyperView1);
            pagePreview.Location = new Point(4, 26);
            pagePreview.Padding = new Padding(3);
            pagePreview.Size = new Size(769, 353);
            pagePreview.Text = "pagePreview";

            hyperView1.AutoScroll = true;
            hyperView1.AutoScrollMinSize = new Size(4, 0);
            hyperView1.BorderStyle = BorderStyle.Fixed3D;
            hyperView1.BorderWidth = 0;
            hyperView1.Dock = DockStyle.Fill;
            hyperView1.LinkColor = Colors.Blue;
            hyperView1.Location = new Point(3, 3);
            hyperView1.Size = new Size(763, 347);

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(797, 457);
            Controls.Add(tabControl1);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            Padding = new Padding(10);
            ShowInTaskbar = false;
            Title = "NoteEditDlg";
            tabControl1.ResumeLayout();
            pageEditor.ResumeLayout();
            pagePreview.ResumeLayout();
            ResumeLayout();
        }
    }
}
