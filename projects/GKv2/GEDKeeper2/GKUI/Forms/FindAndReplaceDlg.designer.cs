namespace GKUI.Forms
{
    partial class FindAndReplaceDlg
    {
        private System.Windows.Forms.Button btnReplace;
        private System.Windows.Forms.Button btnReplaceAll;
        private System.Windows.Forms.GroupBox gbFilters;
        private System.Windows.Forms.Label lblPattern;
        private System.Windows.Forms.ComboBox cmbPattern;
        private System.Windows.Forms.CheckBox chkMatchCase;
        private System.Windows.Forms.CheckBox chkMatchWildcards;
        private System.Windows.Forms.Label lblReplacement;
        private System.Windows.Forms.ComboBox cmbReplacement;
        private System.Windows.Forms.CheckBox chkWholeWord;
        private System.Windows.Forms.Label lblProperty;
        private System.Windows.Forms.ComboBox cmbProperty;
        private System.Windows.Forms.Label lblRecord;
        private System.Windows.Forms.ComboBox cmbRecord;
        private System.Windows.Forms.Button btnPrev;
        private System.Windows.Forms.Button btnNext;
        
        private void InitializeComponent()
        {
            this.btnReplace = new System.Windows.Forms.Button();
            this.btnReplaceAll = new System.Windows.Forms.Button();
            this.gbFilters = new System.Windows.Forms.GroupBox();
            this.lblProperty = new System.Windows.Forms.Label();
            this.cmbProperty = new System.Windows.Forms.ComboBox();
            this.lblRecord = new System.Windows.Forms.Label();
            this.cmbRecord = new System.Windows.Forms.ComboBox();
            this.lblPattern = new System.Windows.Forms.Label();
            this.cmbPattern = new System.Windows.Forms.ComboBox();
            this.chkMatchCase = new System.Windows.Forms.CheckBox();
            this.chkMatchWildcards = new System.Windows.Forms.CheckBox();
            this.lblReplacement = new System.Windows.Forms.Label();
            this.cmbReplacement = new System.Windows.Forms.ComboBox();
            this.chkWholeWord = new System.Windows.Forms.CheckBox();
            this.btnPrev = new System.Windows.Forms.Button();
            this.btnNext = new System.Windows.Forms.Button();
            this.gbFilters.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnReplace
            // 
            this.btnReplace.Location = new System.Drawing.Point(417, 170);
            this.btnReplace.Margin = new System.Windows.Forms.Padding(2);
            this.btnReplace.Name = "btnReplace";
            this.btnReplace.Size = new System.Drawing.Size(112, 30);
            this.btnReplace.TabIndex = 14;
            this.btnReplace.Text = "btnReplace";
            this.btnReplace.Click += new System.EventHandler(this.btnReplace_Click);
            // 
            // btnReplaceAll
            // 
            this.btnReplaceAll.Location = new System.Drawing.Point(539, 170);
            this.btnReplaceAll.Margin = new System.Windows.Forms.Padding(2);
            this.btnReplaceAll.Name = "btnReplaceAll";
            this.btnReplaceAll.Size = new System.Drawing.Size(112, 30);
            this.btnReplaceAll.TabIndex = 15;
            this.btnReplaceAll.Text = "btnReplaceAll";
            this.btnReplaceAll.Click += new System.EventHandler(this.btnReplaceAll_Click);
            // 
            // gbFilters
            // 
            this.gbFilters.Controls.Add(this.lblProperty);
            this.gbFilters.Controls.Add(this.cmbProperty);
            this.gbFilters.Controls.Add(this.lblRecord);
            this.gbFilters.Controls.Add(this.cmbRecord);
            this.gbFilters.Location = new System.Drawing.Point(278, 69);
            this.gbFilters.Margin = new System.Windows.Forms.Padding(2);
            this.gbFilters.Name = "gbFilters";
            this.gbFilters.Padding = new System.Windows.Forms.Padding(2);
            this.gbFilters.Size = new System.Drawing.Size(373, 85);
            this.gbFilters.TabIndex = 7;
            this.gbFilters.TabStop = false;
            // 
            // lblProperty
            // 
            this.lblProperty.AutoSize = true;
            this.lblProperty.Location = new System.Drawing.Point(6, 53);
            this.lblProperty.Margin = new System.Windows.Forms.Padding(2, 2, 2, 0);
            this.lblProperty.Name = "lblProperty";
            this.lblProperty.Size = new System.Drawing.Size(74, 17);
            this.lblProperty.TabIndex = 10;
            this.lblProperty.Text = "lblProperty";
            // 
            // cmbProperty
            // 
            this.cmbProperty.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbProperty.Location = new System.Drawing.Point(145, 50);
            this.cmbProperty.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.cmbProperty.Name = "cmbProperty";
            this.cmbProperty.Size = new System.Drawing.Size(213, 25);
            this.cmbProperty.TabIndex = 11;
            // 
            // lblRecord
            // 
            this.lblRecord.AutoSize = true;
            this.lblRecord.Location = new System.Drawing.Point(6, 24);
            this.lblRecord.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblRecord.Name = "lblRecord";
            this.lblRecord.Size = new System.Drawing.Size(64, 17);
            this.lblRecord.TabIndex = 8;
            this.lblRecord.Text = "lblRecord";
            // 
            // cmbRecord
            // 
            this.cmbRecord.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbRecord.Location = new System.Drawing.Point(145, 21);
            this.cmbRecord.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.cmbRecord.Name = "cmbRecord";
            this.cmbRecord.Size = new System.Drawing.Size(213, 25);
            this.cmbRecord.TabIndex = 9;
            // 
            // lblPattern
            // 
            this.lblPattern.AutoSize = true;
            this.lblPattern.Location = new System.Drawing.Point(11, 14);
            this.lblPattern.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblPattern.Name = "lblPattern";
            this.lblPattern.Size = new System.Drawing.Size(65, 17);
            this.lblPattern.TabIndex = 0;
            this.lblPattern.Text = "lblPattern";
            // 
            // cmbPattern
            // 
            this.cmbPattern.Location = new System.Drawing.Point(203, 11);
            this.cmbPattern.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.cmbPattern.Name = "cmbPattern";
            this.cmbPattern.Size = new System.Drawing.Size(448, 25);
            this.cmbPattern.TabIndex = 1;
            // 
            // chkMatchCase
            // 
            this.chkMatchCase.AutoSize = true;
            this.chkMatchCase.Location = new System.Drawing.Point(17, 80);
            this.chkMatchCase.Margin = new System.Windows.Forms.Padding(2);
            this.chkMatchCase.Name = "chkMatchCase";
            this.chkMatchCase.Size = new System.Drawing.Size(118, 21);
            this.chkMatchCase.TabIndex = 4;
            this.chkMatchCase.Text = "chkMatchCase";
            // 
            // chkMatchWildcards
            // 
            this.chkMatchWildcards.AutoSize = true;
            this.chkMatchWildcards.Location = new System.Drawing.Point(17, 105);
            this.chkMatchWildcards.Margin = new System.Windows.Forms.Padding(2);
            this.chkMatchWildcards.Name = "chkMatchWildcards";
            this.chkMatchWildcards.Size = new System.Drawing.Size(148, 21);
            this.chkMatchWildcards.TabIndex = 5;
            this.chkMatchWildcards.Text = "chkMatchWildcards";
            // 
            // lblReplacement
            // 
            this.lblReplacement.AutoSize = true;
            this.lblReplacement.Location = new System.Drawing.Point(11, 43);
            this.lblReplacement.Margin = new System.Windows.Forms.Padding(2, 2, 2, 0);
            this.lblReplacement.Name = "lblReplacement";
            this.lblReplacement.Size = new System.Drawing.Size(99, 17);
            this.lblReplacement.TabIndex = 2;
            this.lblReplacement.Text = "lblReplacement";
            // 
            // cmbReplacement
            // 
            this.cmbReplacement.Location = new System.Drawing.Point(203, 40);
            this.cmbReplacement.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.cmbReplacement.Name = "cmbReplacement";
            this.cmbReplacement.Size = new System.Drawing.Size(448, 25);
            this.cmbReplacement.TabIndex = 3;
            // 
            // chkWholeWord
            // 
            this.chkWholeWord.AutoSize = true;
            this.chkWholeWord.Location = new System.Drawing.Point(17, 130);
            this.chkWholeWord.Margin = new System.Windows.Forms.Padding(2);
            this.chkWholeWord.Name = "chkWholeWord";
            this.chkWholeWord.Size = new System.Drawing.Size(126, 21);
            this.chkWholeWord.TabIndex = 6;
            this.chkWholeWord.Text = "chkWholeWord";
            // 
            // btnPrev
            // 
            this.btnPrev.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnPrev.Location = new System.Drawing.Point(11, 170);
            this.btnPrev.Margin = new System.Windows.Forms.Padding(2);
            this.btnPrev.Name = "btnPrev";
            this.btnPrev.Size = new System.Drawing.Size(142, 30);
            this.btnPrev.TabIndex = 12;
            this.btnPrev.Text = "btnPrev";
            this.btnPrev.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnPrev.Click += new System.EventHandler(this.btnPrev_Click);
            // 
            // btnNext
            // 
            this.btnNext.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnNext.Location = new System.Drawing.Point(163, 170);
            this.btnNext.Margin = new System.Windows.Forms.Padding(2);
            this.btnNext.Name = "btnNext";
            this.btnNext.Size = new System.Drawing.Size(142, 30);
            this.btnNext.TabIndex = 13;
            this.btnNext.Text = "btnNext";
            this.btnNext.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnNext.Click += new System.EventHandler(this.btnNext_Click);
            // 
            // FindAndReplaceDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(663, 214);
            this.Controls.Add(this.btnPrev);
            this.Controls.Add(this.btnNext);
            this.Controls.Add(this.chkWholeWord);
            this.Controls.Add(this.lblReplacement);
            this.Controls.Add(this.cmbReplacement);
            this.Controls.Add(this.lblPattern);
            this.Controls.Add(this.cmbPattern);
            this.Controls.Add(this.chkMatchCase);
            this.Controls.Add(this.chkMatchWildcards);
            this.Controls.Add(this.btnReplace);
            this.Controls.Add(this.btnReplaceAll);
            this.Controls.Add(this.gbFilters);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(Form_KeyDown);
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FindAndReplaceDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.TopMost = true;
            this.Text = "FindAndReplaceDlg";
            this.gbFilters.ResumeLayout(false);
            this.gbFilters.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }
    }
}
