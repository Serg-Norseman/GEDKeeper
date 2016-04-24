namespace GKUI.Dialogs
{
    partial class TfmPersonsFilter
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
        	this.tsSpecificFilter.Location = new System.Drawing.Point(4, 22);
        	this.tsSpecificFilter.Name = "tsSpecificFilter";
        	this.tsSpecificFilter.Size = new System.Drawing.Size(558, 375);
        	this.tsSpecificFilter.TabIndex = 1;
        	this.tsSpecificFilter.Text = "Специфичный фильтр";
        	// 
        	// Label1
        	// 
        	this.Label1.Location = new System.Drawing.Point(14, 139);
        	this.Label1.Name = "Label1";
        	this.Label1.Size = new System.Drawing.Size(75, 13);
        	this.Label1.TabIndex = 19;
        	this.Label1.Text = "Маска имени";
        	// 
        	// Label3
        	// 
        	this.Label3.Location = new System.Drawing.Point(14, 179);
        	this.Label3.Name = "Label3";
        	this.Label3.Size = new System.Drawing.Size(175, 13);
        	this.Label3.TabIndex = 21;
        	this.Label3.Text = "Маска местожительства";
        	// 
        	// Label4
        	// 
        	this.Label4.Location = new System.Drawing.Point(14, 259);
        	this.Label4.Name = "Label4";
        	this.Label4.Size = new System.Drawing.Size(45, 13);
        	this.Label4.TabIndex = 25;
        	this.Label4.Text = "Группы";
        	// 
        	// GroupBox1
        	// 
        	this.GroupBox1.Controls.Add(this.CheckPatriarch);
        	this.GroupBox1.Location = new System.Drawing.Point(328, 78);
        	this.GroupBox1.Name = "GroupBox1";
        	this.GroupBox1.Size = new System.Drawing.Size(188, 41);
        	this.GroupBox1.TabIndex = 29;
        	this.GroupBox1.TabStop = false;
        	// 
        	// CheckPatriarch
        	// 
        	this.CheckPatriarch.Location = new System.Drawing.Point(8, 16);
        	this.CheckPatriarch.Name = "CheckPatriarch";
        	this.CheckPatriarch.Size = new System.Drawing.Size(159, 17);
        	this.CheckPatriarch.TabIndex = 0;
        	this.CheckPatriarch.Text = "Только главы семей";
        	// 
        	// Label5
        	// 
        	this.Label5.Location = new System.Drawing.Point(14, 299);
        	this.Label5.Name = "Label5";
        	this.Label5.Size = new System.Drawing.Size(60, 13);
        	this.Label5.TabIndex = 27;
        	this.Label5.Text = "Источники";
        	// 
        	// Label2
        	// 
        	this.Label2.Location = new System.Drawing.Point(328, 22);
        	this.Label2.Name = "Label2";
        	this.Label2.Size = new System.Drawing.Size(70, 13);
        	this.Label2.TabIndex = 17;
        	this.Label2.Text = "В живых до:";
        	// 
        	// Label6
        	// 
        	this.Label6.Location = new System.Drawing.Point(14, 219);
        	this.Label6.Name = "Label6";
        	this.Label6.Size = new System.Drawing.Size(85, 13);
        	this.Label6.TabIndex = 23;
        	this.Label6.Text = "Маска фактов";
        	// 
        	// rgLife
        	// 
        	this.rgLife.Controls.Add(this.RadioButton4);
        	this.rgLife.Controls.Add(this.RadioButton3);
        	this.rgLife.Controls.Add(this.RadioButton2);
        	this.rgLife.Controls.Add(this.RadioButton1);
        	this.rgLife.Location = new System.Drawing.Point(170, 15);
        	this.rgLife.Name = "rgLife";
        	this.rgLife.Size = new System.Drawing.Size(137, 104);
        	this.rgLife.TabIndex = 15;
        	this.rgLife.TabStop = false;
        	// 
        	// RadioButton4
        	// 
        	this.RadioButton4.Location = new System.Drawing.Point(8, 74);
        	this.RadioButton4.Name = "RadioButton4";
        	this.RadioButton4.Size = new System.Drawing.Size(114, 24);
        	this.RadioButton4.TabIndex = 3;
        	this.RadioButton4.Text = "в живых до";
        	this.RadioButton4.Click += new System.EventHandler(this.rgLifeClick);
        	// 
        	// RadioButton3
        	// 
        	this.RadioButton3.Location = new System.Drawing.Point(8, 56);
        	this.RadioButton3.Name = "RadioButton3";
        	this.RadioButton3.Size = new System.Drawing.Size(114, 24);
        	this.RadioButton3.TabIndex = 2;
        	this.RadioButton3.Text = "только умершие";
        	this.RadioButton3.Click += new System.EventHandler(this.rgLifeClick);
        	// 
        	// RadioButton2
        	// 
        	this.RadioButton2.Location = new System.Drawing.Point(8, 38);
        	this.RadioButton2.Name = "RadioButton2";
        	this.RadioButton2.Size = new System.Drawing.Size(114, 24);
        	this.RadioButton2.TabIndex = 1;
        	this.RadioButton2.Text = "только живые";
        	this.RadioButton2.Click += new System.EventHandler(this.rgLifeClick);
        	// 
        	// RadioButton1
        	// 
        	this.RadioButton1.Location = new System.Drawing.Point(8, 20);
        	this.RadioButton1.Name = "RadioButton1";
        	this.RadioButton1.Size = new System.Drawing.Size(104, 24);
        	this.RadioButton1.TabIndex = 0;
        	this.RadioButton1.Text = "все";
        	this.RadioButton1.Click += new System.EventHandler(this.rgLifeClick);
        	// 
        	// edName
        	// 
        	this.edName.Location = new System.Drawing.Point(14, 155);
        	this.edName.Name = "edName";
        	this.edName.Size = new System.Drawing.Size(281, 21);
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
        	this.rgSex.Size = new System.Drawing.Size(137, 104);
        	this.rgSex.TabIndex = 16;
        	this.rgSex.TabStop = false;
        	// 
        	// RadioButton7
        	// 
        	this.RadioButton7.Location = new System.Drawing.Point(8, 72);
        	this.RadioButton7.Name = "RadioButton7";
        	this.RadioButton7.Size = new System.Drawing.Size(114, 24);
        	this.RadioButton7.TabIndex = 2;
        	this.RadioButton7.Text = "только женщины";
        	// 
        	// RadioButton5
        	// 
        	this.RadioButton5.Location = new System.Drawing.Point(8, 24);
        	this.RadioButton5.Name = "RadioButton5";
        	this.RadioButton5.Size = new System.Drawing.Size(104, 24);
        	this.RadioButton5.TabIndex = 0;
        	this.RadioButton5.Text = "все";
        	// 
        	// RadioButton6
        	// 
        	this.RadioButton6.Location = new System.Drawing.Point(8, 48);
        	this.RadioButton6.Name = "RadioButton6";
        	this.RadioButton6.Size = new System.Drawing.Size(114, 24);
        	this.RadioButton6.TabIndex = 1;
        	this.RadioButton6.Text = "только мужчины";
        	// 
        	// edAliveBeforeDate
        	// 
        	this.edAliveBeforeDate.Enabled = false;
        	this.edAliveBeforeDate.Location = new System.Drawing.Point(328, 38);
        	this.edAliveBeforeDate.Mask = "00/00/0000";
        	this.edAliveBeforeDate.Name = "edAliveBeforeDate";
        	this.edAliveBeforeDate.Size = new System.Drawing.Size(137, 21);
        	this.edAliveBeforeDate.TabIndex = 18;
        	this.edAliveBeforeDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
        	// 
        	// cbResidence
        	// 
        	this.cbResidence.Location = new System.Drawing.Point(14, 195);
        	this.cbResidence.Name = "cbResidence";
        	this.cbResidence.Size = new System.Drawing.Size(281, 21);
        	this.cbResidence.Sorted = true;
        	this.cbResidence.TabIndex = 22;
        	this.cbResidence.Text = "*";
        	// 
        	// cbGroup
        	// 
        	this.cbGroup.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
        	this.cbGroup.Location = new System.Drawing.Point(14, 275);
        	this.cbGroup.Name = "cbGroup";
        	this.cbGroup.Size = new System.Drawing.Size(281, 21);
        	this.cbGroup.TabIndex = 26;
        	// 
        	// cbSource
        	// 
        	this.cbSource.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
        	this.cbSource.Location = new System.Drawing.Point(14, 315);
        	this.cbSource.Name = "cbSource";
        	this.cbSource.Size = new System.Drawing.Size(281, 21);
        	this.cbSource.TabIndex = 28;
        	// 
        	// cbEventVal
        	// 
        	this.cbEventVal.Location = new System.Drawing.Point(14, 235);
        	this.cbEventVal.Name = "cbEventVal";
        	this.cbEventVal.Size = new System.Drawing.Size(281, 21);
        	this.cbEventVal.Sorted = true;
        	this.cbEventVal.TabIndex = 24;
        	this.cbEventVal.Text = "*";
        	// 
        	// TfmPersonsFilter
        	// 
        	this.ClientSize = new System.Drawing.Size(581, 456);
        	this.Name = "TfmPersonsFilter";
        	this.PageControl1.ResumeLayout(false);
        	this.tsSpecificFilter.ResumeLayout(false);
        	this.tsSpecificFilter.PerformLayout();
        	this.GroupBox1.ResumeLayout(false);
        	this.rgLife.ResumeLayout(false);
        	this.rgSex.ResumeLayout(false);
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
