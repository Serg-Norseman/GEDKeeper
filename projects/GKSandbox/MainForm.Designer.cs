namespace GKSandbox
{
    partial class MainForm
    {
        /// <summary>
        /// Designer variable used to keep track of non-visual components.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        
        /// <summary>
        /// Disposes resources used by the form.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        
        /// <summary>
        /// This method is required for Windows Forms designer support.
        /// Do not change the method contents inside the source code editor. The Forms designer might
        /// not be able to load this method if it was changed manually.
        /// </summary>
        private void InitializeComponent()
        {
            this.gkComboBox1 = new GKCommon.Controls.GKComboBox();
            this.button1 = new System.Windows.Forms.Button();
            this.culturePicker1 = new GKIntl.CulturePicker();
            this.optionsPicker1 = new GKCommon.Controls.OptionsPicker();
            this.SuspendLayout();
            // 
            // gkComboBox1
            // 
            this.gkComboBox1.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
            this.gkComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.gkComboBox1.FormattingEnabled = true;
            this.gkComboBox1.Location = new System.Drawing.Point(12, 12);
            this.gkComboBox1.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.gkComboBox1.Name = "gkComboBox1";
            this.gkComboBox1.Size = new System.Drawing.Size(289, 23);
            this.gkComboBox1.TabIndex = 0;
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(12, 121);
            this.button1.Margin = new System.Windows.Forms.Padding(4);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(291, 28);
            this.button1.TabIndex = 1;
            this.button1.Text = "MediaPlayer Test";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.Button1_Click);
            // 
            // culturePicker1
            // 
            this.culturePicker1.AnchorSize = new System.Drawing.Size(314, 28);
            this.culturePicker1.BackColor = System.Drawing.Color.White;
            this.culturePicker1.Location = new System.Drawing.Point(421, 20);
            this.culturePicker1.Margin = new System.Windows.Forms.Padding(4);
            this.culturePicker1.Name = "culturePicker1";
            this.culturePicker1.Size = new System.Drawing.Size(314, 28);
            this.culturePicker1.TabIndex = 2;
            // 
            // optionsPicker1
            // 
            this.optionsPicker1.AnchorSize = new System.Drawing.Size(315, 31);
            this.optionsPicker1.BackColor = System.Drawing.Color.White;
            this.optionsPicker1.Location = new System.Drawing.Point(420, 80);
            this.optionsPicker1.Margin = new System.Windows.Forms.Padding(4);
            this.optionsPicker1.Name = "optionsPicker1";
            this.optionsPicker1.Size = new System.Drawing.Size(315, 31);
            this.optionsPicker1.TabIndex = 3;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1021, 587);
            this.Controls.Add(this.optionsPicker1);
            this.Controls.Add(this.culturePicker1);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.gkComboBox1);
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "MainForm";
            this.Text = "GKSandbox";
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.MainFormFormClosed);
            this.ResumeLayout(false);
        }
        private GKCommon.Controls.OptionsPicker optionsPicker1;
        private GKIntl.CulturePicker culturePicker1;
        private System.Windows.Forms.Button button1;
        private GKCommon.Controls.GKComboBox gkComboBox1;
    }
}
