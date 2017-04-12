namespace GKUI.Components
{
	partial class GKMergeControl
	{
		/// <summary>
		/// Designer variable used to keep track of non-visual components.
		/// </summary>
		private System.ComponentModel.IContainer components = null;
		
		/// <summary>
		/// Disposes resources used by the control.
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
		    this.Lab1 = new System.Windows.Forms.Label();
		    this.Lab2 = new System.Windows.Forms.Label();
		    this.Edit1 = new System.Windows.Forms.TextBox();
		    this.Edit2 = new System.Windows.Forms.TextBox();
		    this.btnRec1Select = new System.Windows.Forms.Button();
		    this.btnRec2Select = new System.Windows.Forms.Button();
		    this.btnMergeToLeft = new System.Windows.Forms.Button();
		    this.btnMergeToRight = new System.Windows.Forms.Button();
		    this.SuspendLayout();
		    // 
		    // Lab1
		    // 
		    this.Lab1.AutoSize = true;
		    this.Lab1.Location = new System.Drawing.Point(14, 13);
		    this.Lab1.Name = "Lab1";
		    this.Lab1.Size = new System.Drawing.Size(40, 17);
		    this.Lab1.TabIndex = 9;
		    this.Lab1.Text = "XXX1";
		    // 
		    // Lab2
		    // 
		    this.Lab2.AutoSize = true;
		    this.Lab2.Location = new System.Drawing.Point(482, 13);
		    this.Lab2.Name = "Lab2";
		    this.Lab2.Size = new System.Drawing.Size(40, 17);
		    this.Lab2.TabIndex = 11;
		    this.Lab2.Text = "XXX2";
		    // 
		    // Edit1
		    // 
		    this.Edit1.Location = new System.Drawing.Point(14, 33);
		    this.Edit1.Name = "Edit1";
		    this.Edit1.ReadOnly = true;
		    this.Edit1.Size = new System.Drawing.Size(366, 24);
		    this.Edit1.TabIndex = 10;
		    // 
		    // Edit2
		    // 
		    this.Edit2.Location = new System.Drawing.Point(482, 33);
		    this.Edit2.Name = "Edit2";
		    this.Edit2.ReadOnly = true;
		    this.Edit2.Size = new System.Drawing.Size(373, 24);
		    this.Edit2.TabIndex = 12;
		    // 
		    // btnRec1Select
		    // 
		    this.btnRec1Select.Location = new System.Drawing.Point(386, 32);
		    this.btnRec1Select.Name = "btnRec1Select";
		    this.btnRec1Select.Size = new System.Drawing.Size(81, 25);
		    this.btnRec1Select.TabIndex = 13;
		    this.btnRec1Select.Text = "btnRec1Select";
		    this.btnRec1Select.Click += new System.EventHandler(this.btnRec1Select_Click);
		    // 
		    // btnRec2Select
		    // 
		    this.btnRec2Select.Location = new System.Drawing.Point(861, 32);
		    this.btnRec2Select.Name = "btnRec2Select";
		    this.btnRec2Select.Size = new System.Drawing.Size(81, 25);
		    this.btnRec2Select.TabIndex = 14;
		    this.btnRec2Select.Text = "btnRec2Select";
		    this.btnRec2Select.Click += new System.EventHandler(this.btnRec2Select_Click);
		    // 
		    // btnMergeToLeft
		    // 
		    this.btnMergeToLeft.Enabled = false;
		    this.btnMergeToLeft.Location = new System.Drawing.Point(386, 371);
		    this.btnMergeToLeft.Name = "btnMergeToLeft";
		    this.btnMergeToLeft.Size = new System.Drawing.Size(81, 25);
		    this.btnMergeToLeft.TabIndex = 15;
		    this.btnMergeToLeft.Text = "<<<";
		    this.btnMergeToLeft.Click += new System.EventHandler(this.btnMergeToLeft_Click);
		    // 
		    // btnMergeToRight
		    // 
		    this.btnMergeToRight.Enabled = false;
		    this.btnMergeToRight.Location = new System.Drawing.Point(482, 371);
		    this.btnMergeToRight.Name = "btnMergeToRight";
		    this.btnMergeToRight.Size = new System.Drawing.Size(81, 25);
		    this.btnMergeToRight.TabIndex = 16;
		    this.btnMergeToRight.Text = ">>>";
		    this.btnMergeToRight.Click += new System.EventHandler(this.btnMergeToRight_Click);
		    // 
		    // GKMergeControl
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.AutoSize = true;
		    this.Controls.Add(this.Lab1);
		    this.Controls.Add(this.Lab2);
		    this.Controls.Add(this.Edit1);
		    this.Controls.Add(this.Edit2);
		    this.Controls.Add(this.btnRec1Select);
		    this.Controls.Add(this.btnRec2Select);
		    this.Controls.Add(this.btnMergeToLeft);
		    this.Controls.Add(this.btnMergeToRight);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.Name = "GKMergeControl";
		    this.Size = new System.Drawing.Size(957, 402);
		    this.ResumeLayout(false);
		    this.PerformLayout();
		}
		private System.Windows.Forms.Button btnMergeToRight;
		private System.Windows.Forms.Button btnMergeToLeft;
		private System.Windows.Forms.Button btnRec2Select;
		private System.Windows.Forms.Button btnRec1Select;
		private System.Windows.Forms.TextBox Edit2;
		private System.Windows.Forms.TextBox Edit1;
		private System.Windows.Forms.Label Lab2;
		private System.Windows.Forms.Label Lab1;
	}
}
