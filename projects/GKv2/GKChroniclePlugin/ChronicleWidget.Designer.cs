namespace GKChroniclePlugin
{
	partial class ChronicleWidget
	{
		private GKUI.Components.GKListView lvEvents;

		private void InitializeComponent()
		{
		    this.lvEvents = new GKUI.Components.GKListView();
		    this.SuspendLayout();
		    // 
		    // lvEvents
		    // 
		    this.lvEvents.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.lvEvents.FullRowSelect = true;
		    this.lvEvents.HideSelection = false;
		    this.lvEvents.Location = new System.Drawing.Point(10, 10);
		    this.lvEvents.Name = "lvEvents";
		    this.lvEvents.Order = BSLib.Design.BSDTypes.SortOrder.None;
		    this.lvEvents.OwnerDraw = true;
		    this.lvEvents.Size = new System.Drawing.Size(729, 397);
		    this.lvEvents.SortColumn = 0;
		    this.lvEvents.TabIndex = 0;
		    this.lvEvents.UseCompatibleStateImageBehavior = false;
		    this.lvEvents.View = System.Windows.Forms.View.Details;
		    // 
		    // ChronicleWidget
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 17F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
		    this.ClientSize = new System.Drawing.Size(749, 417);
		    this.Controls.Add(this.lvEvents);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
		    this.Name = "ChronicleWidget";
		    this.Padding = new System.Windows.Forms.Padding(10);
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "ChronicleWidget";
		    this.TopMost = true;
		    this.Closed += new System.EventHandler(this.CalcWidget_Closed);
		    this.Load += new System.EventHandler(this.CalcWidget_Load);
		    this.ResumeLayout(false);
		}
	}
}