namespace GKUI.Forms
{
    partial class NoteEditDlgEx
    {
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.RichTextBox txtNote;
        private GKUI.Components.HyperView hyperView1;
        private System.Windows.Forms.ToolStripComboBox cmbSizes;
        private System.Windows.Forms.ToolStripMenuItem miClear;
        private System.Windows.Forms.ToolStripMenuItem miExport;
        private System.Windows.Forms.ToolStripMenuItem miImport;
        private System.Windows.Forms.ToolStripMenuItem miSelectAndCopy;
        private System.Windows.Forms.ToolStripDropDownButton ddbtnActions;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton btnURL;
        private System.Windows.Forms.ToolStripButton btnUnderline;
        private System.Windows.Forms.ToolStripButton btnItalic;
        private System.Windows.Forms.ToolStripButton btnBold;
        private System.Windows.Forms.TabPage pagePreview;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.TabPage pageEditor;
        private GKUI.Components.GKTabControl tabControl1;

        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.tabControl1 = new GKUI.Components.GKTabControl();
            this.pageEditor = new System.Windows.Forms.TabPage();
            this.txtNote = new System.Windows.Forms.RichTextBox();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.btnBold = new System.Windows.Forms.ToolStripButton();
            this.btnItalic = new System.Windows.Forms.ToolStripButton();
            this.btnUnderline = new System.Windows.Forms.ToolStripButton();
            this.btnURL = new System.Windows.Forms.ToolStripButton();
            this.cmbSizes = new System.Windows.Forms.ToolStripComboBox();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.ddbtnActions = new System.Windows.Forms.ToolStripDropDownButton();
            this.miSelectAndCopy = new System.Windows.Forms.ToolStripMenuItem();
            this.miImport = new System.Windows.Forms.ToolStripMenuItem();
            this.miExport = new System.Windows.Forms.ToolStripMenuItem();
            this.miClear = new System.Windows.Forms.ToolStripMenuItem();
            this.pagePreview = new System.Windows.Forms.TabPage();
            this.hyperView1 = new GKUI.Components.HyperView();
            this.tabControl1.SuspendLayout();
            this.pageEditor.SuspendLayout();
            this.toolStrip1.SuspendLayout();
            this.pagePreview.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(555, 406);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(10);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(111, 31);
            this.btnAccept.TabIndex = 1;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(676, 406);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(10);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(111, 31);
            this.btnCancel.TabIndex = 2;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.pageEditor);
            this.tabControl1.Controls.Add(this.pagePreview);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Top;
            this.tabControl1.Location = new System.Drawing.Point(10, 10);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(777, 383);
            this.tabControl1.TabIndex = 3;
            this.tabControl1.SelectedIndexChanged += new System.EventHandler(this.tabControl1_SelectedIndexChanged);
            // 
            // pageEditor
            // 
            this.pageEditor.BackColor = System.Drawing.SystemColors.Control;
            this.pageEditor.Controls.Add(this.txtNote);
            this.pageEditor.Controls.Add(this.toolStrip1);
            this.pageEditor.Location = new System.Drawing.Point(4, 26);
            this.pageEditor.Name = "pageEditor";
            this.pageEditor.Padding = new System.Windows.Forms.Padding(3);
            this.pageEditor.Size = new System.Drawing.Size(769, 353);
            this.pageEditor.TabIndex = 0;
            this.pageEditor.Text = "pageEditor";
            // 
            // txtNote
            // 
            this.txtNote.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txtNote.Location = new System.Drawing.Point(3, 31);
            this.txtNote.Margin = new System.Windows.Forms.Padding(4);
            this.txtNote.Multiline = true;
            this.txtNote.Name = "txtNote";
            this.txtNote.ScrollBars = System.Windows.Forms.RichTextBoxScrollBars.Both;
            this.txtNote.Size = new System.Drawing.Size(763, 319);
            this.txtNote.TabIndex = 1;
            // 
            // toolStrip1
            // 
            this.toolStrip1.Font = new System.Drawing.Font("Tahoma", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                this.btnBold,
                this.btnItalic,
                this.btnUnderline,
                this.btnURL,
                this.cmbSizes,
                this.toolStripSeparator1,
                this.ddbtnActions
            });
            this.toolStrip1.Location = new System.Drawing.Point(3, 3);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(763, 28);
            this.toolStrip1.TabIndex = 0;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // btnBold
            // 
            this.btnBold.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnBold.Font = new System.Drawing.Font("Tahoma", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.btnBold.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnBold.Name = "btnBold";
            this.btnBold.Size = new System.Drawing.Size(23, 25);
            this.btnBold.Text = "B";
            this.btnBold.Click += new System.EventHandler(this.btnBold_Click);
            // 
            // btnItalic
            // 
            this.btnItalic.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnItalic.Font = new System.Drawing.Font("Tahoma", 9F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.btnItalic.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnItalic.Name = "btnItalic";
            this.btnItalic.Size = new System.Drawing.Size(23, 25);
            this.btnItalic.Text = "I";
            this.btnItalic.Click += new System.EventHandler(this.btnItalic_Click);
            // 
            // btnUnderline
            // 
            this.btnUnderline.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnUnderline.Font = new System.Drawing.Font("Tahoma", 9F, System.Drawing.FontStyle.Underline, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.btnUnderline.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnUnderline.Name = "btnUnderline";
            this.btnUnderline.Size = new System.Drawing.Size(23, 25);
            this.btnUnderline.Text = "U";
            this.btnUnderline.Click += new System.EventHandler(this.btnUnderline_Click);
            // 
            // btnURL
            // 
            this.btnURL.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnURL.Font = new System.Drawing.Font("Tahoma", 9F, System.Drawing.FontStyle.Underline, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.btnURL.ForeColor = System.Drawing.Color.Blue;
            this.btnURL.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnURL.Name = "btnURL";
            this.btnURL.Size = new System.Drawing.Size(38, 25);
            this.btnURL.Text = "URL";
            this.btnURL.Click += new System.EventHandler(this.btnURL_Click);
            // 
            // cmbSizes
            // 
            this.cmbSizes.Name = "cmbSizes";
            this.cmbSizes.Size = new System.Drawing.Size(121, 28);
            this.cmbSizes.SelectedIndexChanged += new System.EventHandler(this.cmbSizes_SelectedIndexChanged);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 28);
            // 
            // ddbtnActions
            // 
            this.ddbtnActions.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.ddbtnActions.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
                this.miSelectAndCopy,
                this.miImport,
                this.miExport,
                this.miClear
            });
            this.ddbtnActions.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ddbtnActions.Name = "ddbtnActions";
            this.ddbtnActions.Size = new System.Drawing.Size(67, 25);
            this.ddbtnActions.Text = "Actions";
            // 
            // miSelectAndCopy
            // 
            this.miSelectAndCopy.Name = "miSelectAndCopy";
            this.miSelectAndCopy.Size = new System.Drawing.Size(187, 22);
            this.miSelectAndCopy.Text = "miSelectAndCopy";
            this.miSelectAndCopy.Click += new System.EventHandler(this.miSelectAndCopy_Click);
            // 
            // miImport
            // 
            this.miImport.Name = "miImport";
            this.miImport.Size = new System.Drawing.Size(187, 22);
            this.miImport.Text = "miImport";
            this.miImport.Click += new System.EventHandler(this.miImport_Click);
            // 
            // miExport
            // 
            this.miExport.Name = "miExport";
            this.miExport.Size = new System.Drawing.Size(187, 22);
            this.miExport.Text = "miExport";
            this.miExport.Click += new System.EventHandler(this.miExport_Click);
            // 
            // miClear
            // 
            this.miClear.Name = "miClear";
            this.miClear.Size = new System.Drawing.Size(187, 22);
            this.miClear.Text = "miClear";
            this.miClear.Click += new System.EventHandler(this.miClear_Click);
            // 
            // pagePreview
            // 
            this.pagePreview.BackColor = System.Drawing.SystemColors.Control;
            this.pagePreview.Controls.Add(this.hyperView1);
            this.pagePreview.Location = new System.Drawing.Point(4, 26);
            this.pagePreview.Name = "pagePreview";
            this.pagePreview.Padding = new System.Windows.Forms.Padding(3);
            this.pagePreview.Size = new System.Drawing.Size(769, 353);
            this.pagePreview.TabIndex = 1;
            this.pagePreview.Text = "pagePreview";
            // 
            // hyperView1
            // 
            this.hyperView1.AutoScroll = true;
            this.hyperView1.AutoScrollMinSize = new System.Drawing.Size(4, 0);
            this.hyperView1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.hyperView1.BorderWidth = 0;
            this.hyperView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.hyperView1.LinkColor = System.Drawing.Color.Blue;
            this.hyperView1.Location = new System.Drawing.Point(3, 3);
            this.hyperView1.Name = "hyperView1";
            this.hyperView1.Size = new System.Drawing.Size(763, 347);
            this.hyperView1.TabIndex = 0;
            this.hyperView1.TabStop = true;
            // 
            // NoteEditDlgEx
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(797, 457);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(4);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "NoteEditDlgEx";
            this.Padding = new System.Windows.Forms.Padding(10);
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "NoteEditDlg";
            this.tabControl1.ResumeLayout(false);
            this.pageEditor.ResumeLayout(false);
            this.pageEditor.PerformLayout();
            this.toolStrip1.ResumeLayout(false);
            this.toolStrip1.PerformLayout();
            this.pagePreview.ResumeLayout(false);
            this.ResumeLayout(false);
        }
    }
}
