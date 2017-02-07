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
            this.SuspendLayout();
            // 
            // gkComboBox1
            // 
            this.gkComboBox1.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
            this.gkComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.gkComboBox1.FormattingEnabled = true;
            this.gkComboBox1.Location = new System.Drawing.Point(85, 93);
            this.gkComboBox1.Name = "gkComboBox1";
            this.gkComboBox1.Size = new System.Drawing.Size(289, 23);
            this.gkComboBox1.TabIndex = 0;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(590, 334);
            this.Controls.Add(this.gkComboBox1);
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "MainForm";
            this.Text = "GKSandbox";
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.MainFormFormClosed);
            this.ResumeLayout(false);
        }
        private GKCommon.Controls.GKComboBox gkComboBox1;
    }
}
