namespace GKUI.Dialogs
{
    partial class PersonsFilterDlg
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.TabPage tsSpecificFilter;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
        	this.tsSpecificFilter = new System.Windows.Forms.TabPage();
        	this.Label1 = new System.Windows.Forms.Label();
        	this.Label3 = new System.Windows.Forms.Label();
        	this.Label4 = new System.Windows.Forms.Label();
        	this.GroupBox1 = new System.Windows.Forms.GroupBox();
        	this.CheckPatriarch = new System.Windows.Forms.CheckBox();
        	this.Label5 = new System.Windows.Forms.Label();
        	this.Label2 = new System.Windows.Forms.Label();
        	this.Label6 = new System.Windows.Forms.Label();
        	this.rgLife = new System.Windows.Forms.GroupBox();
        	this.RadioButton4 = new System.Windows.Forms.RadioButton();
        	this.RadioButton3 = new System.Windows.Forms.RadioButton();
        	this.RadioButton2 = new System.Windows.Forms.RadioButton();
        	this.RadioButton1 = new System.Windows.Forms.RadioButton();
        	this.edName = new System.Windows.Forms.ComboBox();
        	this.rgSex = new System.Windows.Forms.GroupBox();
        	this.RadioButton7 = new System.Windows.Forms.RadioButton();
        	this.RadioButton5 = new System.Windows.Forms.RadioButton();
        	this.RadioButton6 = new System.Windows.Forms.RadioButton();
        	this.edAliveBeforeDate = new System.Windows.Forms.MaskedTextBox();
        	this.cbResidence = new System.Windows.Forms.ComboBox();
        	this.cbGroup = new System.Windows.Forms.ComboBox();
        	this.cbSource = new System.Windows.Forms.ComboBox();
        	this.cbEventVal = new System.Windows.Forms.ComboBox();
        	this.PageControl1.SuspendLayout();
        	this.tsSpecificFilter.SuspendLayout();
        	this.GroupBox1.SuspendLayout();
        	this.rgLife.SuspendLayout();
        	this.rgSex.SuspendLayout();
        	this.SuspendLayout();
        	// 
        	// PageControl1
        	// 
        	this.PageControl1.Controls.Add(this.tsSpecificFilter);
        	this.PageControl1.Controls.SetChildIndex(this.tsSpecificFilter, 0);
        	// 
        	// tsSpecificFilter
        	// 
        	this.tsSpecificFilter.Controls.Add(this.Label1);
        	this.tsSpecificFilter.Controls.Add(this.Label3);
        	this.tsSpecificFilter.Controls.Add(this.Label4);
        	this.tsSpecificFilter.Controls.Add(this.GroupBox1);
        	this.tsSpecificFilter.Controls.Add(this.Label5);
        	this.tsSpecificFilter.Controls.Add(this.Label2);
        	this.tsSpecificFilter.Controls.Add(this.Label6);
        	this.tsSpecificFilter.Controls.Add(this.rgLife);
        	this.tsSpecificFilter.Controls.Add(this.edName);
        	this.tsSpecificFilter.Controls.Add(this.rgSex);
        	this.tsSpecificFilter.Controls.Add(this.edAliveBeforeDate);
        	this.tsSpecificFilter.Controls.Add(this.cbResidence);
        	this.tsSpecificFilter.Controls.Add(this.cbGroup);
        	this.tsSpecificFilter.Controls.Add(this.cbSource);
        	this.tsSpecificFilter.Controls.Add(this.cbEventVal);
        	this.tsSpecificFilter.Location = new System.Drawing.Point(4, 26);
        	this.tsSpecificFilter.Name = "tsSpecificFilter";
        	this.tsSpecificFilter.Size = new System.Drawing.Size(785, 457);
        	this.tsSpecificFilter.TabIndex = 1;
        	this.tsSpecificFilter.Text = "Специфичный фильтр";
        	// 
        	// Label1
        	// 
        	this.Label1.AutoSize = true;
        	this.Label1.Location = new System.Drawing.Point(14, 160);
        	this.Label1.Name = "Label1";
        	this.Label1.Size = new System.Drawing.Size(90, 17);
        	this.Label1.TabIndex = 19;
        	this.Label1.Text = "Маска имени";
        	// 
        	// Label3
        	// 
        	this.Label3.AutoSize = true;
        	this.Label3.Location = new System.Drawing.Point(14, 209);
        	this.Label3.Name = "Label3";
        	this.Label3.Size = new System.Drawing.Size(166, 17);
        	this.Label3.TabIndex = 21;
        	this.Label3.Text = "Маска местожительства";
        	// 
        	// Label4
        	// 
        	this.Label4.AutoSize = true;
        	this.Label4.Location = new System.Drawing.Point(14, 309);
        	this.Label4.Name = "Label4";
        	this.Label4.Size = new System.Drawing.Size(57, 17);
        	this.Label4.TabIndex = 25;
        	this.Label4.Text = "Группы";
        	// 
        	// GroupBox1
        	// 
        	this.GroupBox1.Controls.Add(this.CheckPatriarch);
        	this.GroupBox1.Location = new System.Drawing.Point(411, 73);
        	this.GroupBox1.Name = "GroupBox1";
        	this.GroupBox1.Size = new System.Drawing.Size(226, 41);
        	this.GroupBox1.TabIndex = 29;
        	this.GroupBox1.TabStop = false;
        	// 
        	// CheckPatriarch
        	// 
        	this.CheckPatriarch.AutoSize = true;
        	this.CheckPatriarch.Location = new System.Drawing.Point(8, 16);
        	this.CheckPatriarch.Name = "CheckPatriarch";
        	this.CheckPatriarch.Size = new System.Drawing.Size(160, 21);
        	this.CheckPatriarch.TabIndex = 0;
        	this.CheckPatriarch.Text = "Только главы семей";
        	// 
        	// Label5
        	// 
        	this.Label5.AutoSize = true;
        	this.Label5.Location = new System.Drawing.Point(14, 359);
        	this.Label5.Name = "Label5";
        	this.Label5.Size = new System.Drawing.Size(79, 17);
        	this.Label5.TabIndex = 27;
        	this.Label5.Text = "Источники";
        	// 
        	// Label2
        	// 
        	this.Label2.AutoSize = true;
        	this.Label2.Location = new System.Drawing.Point(411, 15);
        	this.Label2.Name = "Label2";
        	this.Label2.Size = new System.Drawing.Size(88, 17);
        	this.Label2.TabIndex = 17;
        	this.Label2.Text = "В живых до:";
        	// 
        	// Label6
        	// 
        	this.Label6.AutoSize = true;
        	this.Label6.Location = new System.Drawing.Point(14, 260);
        	this.Label6.Name = "Label6";
        	this.Label6.Size = new System.Drawing.Size(97, 17);
        	this.Label6.TabIndex = 23;
        	this.Label6.Text = "Маска фактов";
        	// 
        	// rgLife
        	// 
        	this.rgLife.Controls.Add(this.RadioButton4);
        	this.rgLife.Controls.Add(this.RadioButton3);
        	this.rgLife.Controls.Add(this.RadioButton2);
        	this.rgLife.Controls.Add(this.RadioButton1);
        	this.rgLife.Location = new System.Drawing.Point(197, 15);
        	this.rgLife.Name = "rgLife";
        	this.rgLife.Size = new System.Drawing.Size(193, 130);
        	this.rgLife.TabIndex = 15;
        	this.rgLife.TabStop = false;
        	// 
        	// RadioButton4
        	// 
        	this.RadioButton4.AutoSize = true;
        	this.RadioButton4.Location = new System.Drawing.Point(8, 101);
        	this.RadioButton4.Name = "RadioButton4";
        	this.RadioButton4.Size = new System.Drawing.Size(103, 21);
        	this.RadioButton4.TabIndex = 3;
        	this.RadioButton4.Text = "в живых до";
        	this.RadioButton4.Click += new System.EventHandler(this.rgLifeClick);
        	// 
        	// RadioButton3
        	// 
        	this.RadioButton3.AutoSize = true;
        	this.RadioButton3.Location = new System.Drawing.Point(8, 74);
        	this.RadioButton3.Name = "RadioButton3";
        	this.RadioButton3.Size = new System.Drawing.Size(136, 21);
        	this.RadioButton3.TabIndex = 2;
        	this.RadioButton3.Text = "только умершие";
        	this.RadioButton3.Click += new System.EventHandler(this.rgLifeClick);
        	// 
        	// RadioButton2
        	// 
        	this.RadioButton2.AutoSize = true;
        	this.RadioButton2.Location = new System.Drawing.Point(8, 47);
        	this.RadioButton2.Name = "RadioButton2";
        	this.RadioButton2.Size = new System.Drawing.Size(121, 21);
        	this.RadioButton2.TabIndex = 1;
        	this.RadioButton2.Text = "только живые";
        	this.RadioButton2.Click += new System.EventHandler(this.rgLifeClick);
        	// 
        	// RadioButton1
        	// 
        	this.RadioButton1.AutoSize = true;
        	this.RadioButton1.Location = new System.Drawing.Point(8, 20);
        	this.RadioButton1.Name = "RadioButton1";
        	this.RadioButton1.Size = new System.Drawing.Size(50, 21);
        	this.RadioButton1.TabIndex = 0;
        	this.RadioButton1.Text = "все";
        	this.RadioButton1.Click += new System.EventHandler(this.rgLifeClick);
        	// 
        	// edName
        	// 
        	this.edName.Location = new System.Drawing.Point(14, 180);
        	this.edName.Name = "edName";
        	this.edName.Size = new System.Drawing.Size(281, 25);
        	this.edName.Sorted = true;
        	this.edName.TabIndex = 20;
        	this.edName.Text = "*";
        	// 
        	// rgSex
        	// 
        	this.rgSex.Controls.Add(this.RadioButton7);
        	this.rgSex.Controls.Add(this.RadioButton5);
        	this.rgSex.Controls.Add(this.RadioButton6);
        	this.rgSex.Location = new System.Drawing.Point(14, 15);
        	this.rgSex.Name = "rgSex";
        	this.rgSex.Size = new System.Drawing.Size(166, 130);
        	this.rgSex.TabIndex = 16;
        	this.rgSex.TabStop = false;
        	// 
        	// RadioButton7
        	// 
        	this.RadioButton7.AutoSize = true;
        	this.RadioButton7.Location = new System.Drawing.Point(8, 78);
        	this.RadioButton7.Name = "RadioButton7";
        	this.RadioButton7.Size = new System.Drawing.Size(141, 21);
        	this.RadioButton7.TabIndex = 2;
        	this.RadioButton7.Text = "только женщины";
        	// 
        	// RadioButton5
        	// 
        	this.RadioButton5.AutoSize = true;
        	this.RadioButton5.Location = new System.Drawing.Point(8, 24);
        	this.RadioButton5.Name = "RadioButton5";
        	this.RadioButton5.Size = new System.Drawing.Size(50, 21);
        	this.RadioButton5.TabIndex = 0;
        	this.RadioButton5.Text = "все";
        	// 
        	// RadioButton6
        	// 
        	this.RadioButton6.AutoSize = true;
        	this.RadioButton6.Location = new System.Drawing.Point(8, 51);
        	this.RadioButton6.Name = "RadioButton6";
        	this.RadioButton6.Size = new System.Drawing.Size(140, 21);
        	this.RadioButton6.TabIndex = 1;
        	this.RadioButton6.Text = "только мужчины";
        	// 
        	// edAliveBeforeDate
        	// 
        	this.edAliveBeforeDate.Enabled = false;
        	this.edAliveBeforeDate.Location = new System.Drawing.Point(411, 35);
        	this.edAliveBeforeDate.Mask = "00/00/0000";
        	this.edAliveBeforeDate.Name = "edAliveBeforeDate";
        	this.edAliveBeforeDate.Size = new System.Drawing.Size(137, 24);
        	this.edAliveBeforeDate.TabIndex = 18;
        	this.edAliveBeforeDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
        	// 
        	// cbResidence
        	// 
        	this.cbResidence.Location = new System.Drawing.Point(14, 229);
        	this.cbResidence.Name = "cbResidence";
        	this.cbResidence.Size = new System.Drawing.Size(281, 25);
        	this.cbResidence.Sorted = true;
        	this.cbResidence.TabIndex = 22;
        	this.cbResidence.Text = "*";
        	// 
        	// cbGroup
        	// 
        	this.cbGroup.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
        	this.cbGroup.Location = new System.Drawing.Point(14, 329);
        	this.cbGroup.Name = "cbGroup";
        	this.cbGroup.Size = new System.Drawing.Size(281, 25);
        	this.cbGroup.TabIndex = 26;
        	// 
        	// cbSource
        	// 
        	this.cbSource.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
        	this.cbSource.Location = new System.Drawing.Point(14, 379);
        	this.cbSource.Name = "cbSource";
        	this.cbSource.Size = new System.Drawing.Size(281, 25);
        	this.cbSource.TabIndex = 28;
        	// 
        	// cbEventVal
        	// 
        	this.cbEventVal.Location = new System.Drawing.Point(14, 280);
        	this.cbEventVal.Name = "cbEventVal";
        	this.cbEventVal.Size = new System.Drawing.Size(281, 25);
        	this.cbEventVal.Sorted = true;
        	this.cbEventVal.TabIndex = 24;
        	this.cbEventVal.Text = "*";
        	// 
        	// PersonsFilterDlg
        	// 
        	this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
        	this.ClientSize = new System.Drawing.Size(816, 557);
        	this.Name = "PersonsFilterDlg";
        	this.PageControl1.ResumeLayout(false);
        	this.tsSpecificFilter.ResumeLayout(false);
        	this.tsSpecificFilter.PerformLayout();
        	this.GroupBox1.ResumeLayout(false);
        	this.GroupBox1.PerformLayout();
        	this.rgLife.ResumeLayout(false);
        	this.rgLife.PerformLayout();
        	this.rgSex.ResumeLayout(false);
        	this.rgSex.PerformLayout();
        	this.ResumeLayout(false);
        }
        private System.Windows.Forms.ComboBox cbEventVal;
        private System.Windows.Forms.ComboBox cbSource;
        private System.Windows.Forms.ComboBox cbGroup;
        private System.Windows.Forms.ComboBox cbResidence;
        private System.Windows.Forms.MaskedTextBox edAliveBeforeDate;
        private System.Windows.Forms.RadioButton RadioButton6;
        private System.Windows.Forms.RadioButton RadioButton5;
        private System.Windows.Forms.RadioButton RadioButton7;
        private System.Windows.Forms.GroupBox rgSex;
        private System.Windows.Forms.ComboBox edName;
        private System.Windows.Forms.RadioButton RadioButton1;
        private System.Windows.Forms.RadioButton RadioButton2;
        private System.Windows.Forms.RadioButton RadioButton3;
        private System.Windows.Forms.RadioButton RadioButton4;
        private System.Windows.Forms.GroupBox rgLife;
        private System.Windows.Forms.Label Label6;
        private System.Windows.Forms.Label Label2;
        private System.Windows.Forms.Label Label5;
        private System.Windows.Forms.CheckBox CheckPatriarch;
        private System.Windows.Forms.GroupBox GroupBox1;
        private System.Windows.Forms.Label Label4;
        private System.Windows.Forms.Label Label3;
        private System.Windows.Forms.Label Label1;

        #endregion
    }
}
