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
            this.SuspendLayout();
            // 
            // gkComboBox1
            // 
            this.gkComboBox1.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
            this.gkComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.gkComboBox1.FormattingEnabled = true;
            this.gkComboBox1.Location = new System.Drawing.Point(9, 10);
            this.gkComboBox1.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.gkComboBox1.Name = "gkComboBox1";
            this.gkComboBox1.Size = new System.Drawing.Size(218, 21);
            this.gkComboBox1.TabIndex = 0;
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(9, 98);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(218, 23);
            this.button1.TabIndex = 1;
            this.button1.Text = "MediaPlayer Test";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.Button1_Click);
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(766, 477);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.gkComboBox1);
            this.Name = "MainForm";
            this.Text = "GKSandbox";
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.MainFormFormClosed);
            this.ResumeLayout(false);
        }
        private System.Windows.Forms.Button button1;
        private GKCommon.Controls.GKComboBox gkComboBox1;
    }
}
