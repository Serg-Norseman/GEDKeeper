using System;

namespace GKUI.Dialogs
{
    partial class MediaEditDlg
    {
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.TabControl tabsData;
        private System.Windows.Forms.TabPage pageNotes;
        private System.Windows.Forms.TabPage pageSources;
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnView;
        private System.Windows.Forms.TabPage pageCommon;
        private System.Windows.Forms.Label lblName;
        private System.Windows.Forms.TextBox txtName;
        private System.Windows.Forms.Label lblType;
        private System.Windows.Forms.ComboBox cmbMediaType;
        private System.Windows.Forms.Label lblStoreType;
        private System.Windows.Forms.ComboBox cmbStoreType;
        private System.Windows.Forms.Label lblFile;
        private System.Windows.Forms.TextBox txtFile;
        private System.Windows.Forms.Button btnFileSelect;
        private System.Windows.Forms.PictureBox picBox;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.Panel panel3;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.btnView = new System.Windows.Forms.Button();
            this.panel1 = new System.Windows.Forms.Panel();
            this.panel3 = new System.Windows.Forms.Panel();
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.tabsData = new System.Windows.Forms.TabControl();
            this.pageCommon = new System.Windows.Forms.TabPage();
            this.picBox = new System.Windows.Forms.PictureBox();
            this.panel2 = new System.Windows.Forms.Panel();
            this.lblName = new System.Windows.Forms.Label();
            this.btnFileSelect = new System.Windows.Forms.Button();
            this.txtFile = new System.Windows.Forms.TextBox();
            this.lblType = new System.Windows.Forms.Label();
            this.cmbStoreType = new System.Windows.Forms.ComboBox();
            this.lblStoreType = new System.Windows.Forms.Label();
            this.cmbMediaType = new System.Windows.Forms.ComboBox();
            this.lblFile = new System.Windows.Forms.Label();
            this.txtName = new System.Windows.Forms.TextBox();
            this.pageNotes = new System.Windows.Forms.TabPage();
            this.pageSources = new System.Windows.Forms.TabPage();
            this.panel1.SuspendLayout();
            this.panel3.SuspendLayout();
            this.tabsData.SuspendLayout();
            this.pageCommon.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.picBox)).BeginInit();
            this.panel2.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnView
            // 
            this.btnView.Location = new System.Drawing.Point(11, 8);
            this.btnView.Margin = new System.Windows.Forms.Padding(2);
            this.btnView.Name = "btnView";
            this.btnView.Size = new System.Drawing.Size(91, 24);
            this.btnView.TabIndex = 1;
            this.btnView.Text = "btnView";
            this.btnView.Click += new System.EventHandler(this.btnView_Click);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.panel3);
            this.panel1.Controls.Add(this.btnView);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(0, 436);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(582, 43);
            this.panel1.TabIndex = 4;
            // 
            // panel3
            // 
            this.panel3.Controls.Add(this.btnAccept);
            this.panel3.Controls.Add(this.btnCancel);
            this.panel3.Dock = System.Windows.Forms.DockStyle.Right;
            this.panel3.Location = new System.Drawing.Point(347, 0);
            this.panel3.Name = "panel3";
            this.panel3.Size = new System.Drawing.Size(235, 43);
            this.panel3.TabIndex = 4;
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(42, 8);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(2);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(90, 24);
            this.btnAccept.TabIndex = 4;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(138, 8);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(90, 24);
            this.btnCancel.TabIndex = 5;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // tabsData
            // 
            this.tabsData.Controls.Add(this.pageCommon);
            this.tabsData.Controls.Add(this.pageNotes);
            this.tabsData.Controls.Add(this.pageSources);
            this.tabsData.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsData.Location = new System.Drawing.Point(0, 0);
            this.tabsData.Margin = new System.Windows.Forms.Padding(2);
            this.tabsData.Name = "tabsData";
            this.tabsData.SelectedIndex = 0;
            this.tabsData.Size = new System.Drawing.Size(582, 436);
            this.tabsData.TabIndex = 5;
            // 
            // pageCommon
            // 
            this.pageCommon.Controls.Add(this.picBox);
            this.pageCommon.Controls.Add(this.panel2);
            this.pageCommon.Location = new System.Drawing.Point(4, 22);
            this.pageCommon.Margin = new System.Windows.Forms.Padding(2);
            this.pageCommon.Name = "pageCommon";
            this.pageCommon.Size = new System.Drawing.Size(574, 410);
            this.pageCommon.TabIndex = 0;
            this.pageCommon.Text = "pageCommon";
            // 
            // picBox
            // 
            this.picBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.picBox.Location = new System.Drawing.Point(0, 144);
            this.picBox.Name = "picBox";
            this.picBox.Size = new System.Drawing.Size(574, 266);
            this.picBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.picBox.TabIndex = 11;
            this.picBox.TabStop = false;
            // 
            // panel2
            // 
            this.panel2.Controls.Add(this.lblName);
            this.panel2.Controls.Add(this.btnFileSelect);
            this.panel2.Controls.Add(this.txtFile);
            this.panel2.Controls.Add(this.lblType);
            this.panel2.Controls.Add(this.cmbStoreType);
            this.panel2.Controls.Add(this.lblStoreType);
            this.panel2.Controls.Add(this.cmbMediaType);
            this.panel2.Controls.Add(this.lblFile);
            this.panel2.Controls.Add(this.txtName);
            this.panel2.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel2.Location = new System.Drawing.Point(0, 0);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(574, 144);
            this.panel2.TabIndex = 10;
            // 
            // lblName
            // 
            this.lblName.AutoSize = true;
            this.lblName.Location = new System.Drawing.Point(7, 7);
            this.lblName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblName.Name = "lblName";
            this.lblName.Size = new System.Drawing.Size(44, 13);
            this.lblName.TabIndex = 0;
            this.lblName.Text = "lblName";
            // 
            // btnFileSelect
            // 
            this.btnFileSelect.Location = new System.Drawing.Point(518, 65);
            this.btnFileSelect.Margin = new System.Windows.Forms.Padding(2);
            this.btnFileSelect.Name = "btnFileSelect";
            this.btnFileSelect.Size = new System.Drawing.Size(48, 20);
            this.btnFileSelect.TabIndex = 4;
            this.btnFileSelect.Text = "...";
            // 
            // txtFile
            // 
            this.txtFile.Location = new System.Drawing.Point(7, 65);
            this.txtFile.Margin = new System.Windows.Forms.Padding(2);
            this.txtFile.Name = "txtFile";
            this.txtFile.ReadOnly = true;
            this.txtFile.Size = new System.Drawing.Size(504, 21);
            this.txtFile.TabIndex = 3;
            // 
            // lblType
            // 
            this.lblType.AutoSize = true;
            this.lblType.Location = new System.Drawing.Point(7, 91);
            this.lblType.Margin = new System.Windows.Forms.Padding(2, 5, 2, 0);
            this.lblType.Name = "lblType";
            this.lblType.Size = new System.Drawing.Size(41, 13);
            this.lblType.TabIndex = 5;
            this.lblType.Text = "lblType";
            // 
            // cmbStoreType
            // 
            this.cmbStoreType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbStoreType.Location = new System.Drawing.Point(213, 107);
            this.cmbStoreType.Margin = new System.Windows.Forms.Padding(2);
            this.cmbStoreType.Name = "cmbStoreType";
            this.cmbStoreType.Size = new System.Drawing.Size(226, 21);
            this.cmbStoreType.TabIndex = 8;
            // 
            // lblStoreType
            // 
            this.lblStoreType.AutoSize = true;
            this.lblStoreType.Location = new System.Drawing.Point(213, 91);
            this.lblStoreType.Margin = new System.Windows.Forms.Padding(2, 5, 2, 0);
            this.lblStoreType.Name = "lblStoreType";
            this.lblStoreType.Size = new System.Drawing.Size(67, 13);
            this.lblStoreType.TabIndex = 7;
            this.lblStoreType.Text = "lblStoreType";
            // 
            // cmbMediaType
            // 
            this.cmbMediaType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbMediaType.DropDownWidth = 15;
            this.cmbMediaType.Location = new System.Drawing.Point(7, 107);
            this.cmbMediaType.Margin = new System.Windows.Forms.Padding(2);
            this.cmbMediaType.Name = "cmbMediaType";
            this.cmbMediaType.Size = new System.Drawing.Size(190, 21);
            this.cmbMediaType.TabIndex = 6;
            // 
            // lblFile
            // 
            this.lblFile.AutoSize = true;
            this.lblFile.Location = new System.Drawing.Point(6, 47);
            this.lblFile.Margin = new System.Windows.Forms.Padding(2, 5, 2, 0);
            this.lblFile.Name = "lblFile";
            this.lblFile.Size = new System.Drawing.Size(33, 13);
            this.lblFile.TabIndex = 2;
            this.lblFile.Text = "lblFile";
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(7, 22);
            this.txtName.Margin = new System.Windows.Forms.Padding(2);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(558, 21);
            this.txtName.TabIndex = 1;
            // 
            // pageNotes
            // 
            this.pageNotes.Location = new System.Drawing.Point(4, 22);
            this.pageNotes.Margin = new System.Windows.Forms.Padding(2);
            this.pageNotes.Name = "pageNotes";
            this.pageNotes.Size = new System.Drawing.Size(574, 410);
            this.pageNotes.TabIndex = 1;
            this.pageNotes.Text = "pageNotes";
            // 
            // pageSources
            // 
            this.pageSources.Location = new System.Drawing.Point(4, 22);
            this.pageSources.Margin = new System.Windows.Forms.Padding(2);
            this.pageSources.Name = "pageSources";
            this.pageSources.Size = new System.Drawing.Size(574, 410);
            this.pageSources.TabIndex = 2;
            this.pageSources.Text = "pageSources";
            // 
            // MediaEditDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(582, 479);
            this.Controls.Add(this.tabsData);
            this.Controls.Add(this.panel1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MinimizeBox = false;
            this.Name = "MediaEditDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "MediaEditDlg";
            this.Shown += new System.EventHandler(this.MediaEditDlgShown);
            this.panel1.ResumeLayout(false);
            this.panel3.ResumeLayout(false);
            this.tabsData.ResumeLayout(false);
            this.pageCommon.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.picBox)).EndInit();
            this.panel2.ResumeLayout(false);
            this.panel2.PerformLayout();
            this.ResumeLayout(false);

        }
    }
}