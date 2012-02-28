using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
namespace AgetoAge4
{
	public class frmAncCircleStyleManager : Form
	{
		private IContainer components;
		private StatusStrip statusStrip1;
		private ToolStripStatusLabel toolStripStatusLabel1;
		private Button button6;
		private ToolStripMenuItem newFileToolStripMenuItem;
		private MenuStrip menuStrip1;
		private ToolStripMenuItem fileToolStripMenuItem;
		private ToolStripMenuItem openToolStripMenuItem;
		private ToolStripMenuItem saveToolStripMenuItem;
		private ToolStripSeparator toolStripSeparator1;
		private ToolStripMenuItem exitToolStripMenuItem;
		private Button button5;
		private Button button4;
		private Button button3;
		private ToolStripMenuItem renameToolStripMenuItem;
		private ContextMenuStrip contextMenuStrip1;
		private ListBox listBox1;
		private SaveFileDialog saveFileDialog1;
		private OpenFileDialog openFileDialog1;
		public FamilyDoc doc;
		public clsAncCircleStyle choosedstyle;
		protected override void Dispose(bool disposing)
		{
			if (disposing && this.components != null)
			{
				this.components.Dispose();
			}
			base.Dispose(disposing);
		}
		private void InitializeComponent()
		{
			this.components = new Container();
			this.statusStrip1 = new StatusStrip();
			this.toolStripStatusLabel1 = new ToolStripStatusLabel();
			this.button6 = new Button();
			this.newFileToolStripMenuItem = new ToolStripMenuItem();
			this.menuStrip1 = new MenuStrip();
			this.fileToolStripMenuItem = new ToolStripMenuItem();
			this.openToolStripMenuItem = new ToolStripMenuItem();
			this.saveToolStripMenuItem = new ToolStripMenuItem();
			this.toolStripSeparator1 = new ToolStripSeparator();
			this.exitToolStripMenuItem = new ToolStripMenuItem();
			this.button5 = new Button();
			this.button4 = new Button();
			this.button3 = new Button();
			this.renameToolStripMenuItem = new ToolStripMenuItem();
			this.contextMenuStrip1 = new ContextMenuStrip(this.components);
			this.listBox1 = new ListBox();
			this.saveFileDialog1 = new SaveFileDialog();
			this.openFileDialog1 = new OpenFileDialog();
			this.statusStrip1.SuspendLayout();
			this.menuStrip1.SuspendLayout();
			this.contextMenuStrip1.SuspendLayout();
			base.SuspendLayout();
			this.statusStrip1.Items.AddRange(new ToolStripItem[]
			{
				this.toolStripStatusLabel1
			});
			this.statusStrip1.Location = new Point(0, 314);
			this.statusStrip1.Name = "statusStrip1";
			this.statusStrip1.Size = new Size(468, 22);
			this.statusStrip1.TabIndex = 13;
			this.statusStrip1.Text = "statusStrip1";
			this.toolStripStatusLabel1.Name = "toolStripStatusLabel1";
			this.toolStripStatusLabel1.Size = new Size(118, 17);
			this.toolStripStatusLabel1.Text = "toolStripStatusLabel1";
			this.button6.Location = new Point(359, 280);
			this.button6.Name = "button6";
			this.button6.Size = new Size(99, 23);
			this.button6.TabIndex = 14;
			this.button6.Text = "Copy to new style";
			this.button6.UseVisualStyleBackColor = true;
			this.button6.Click += new EventHandler(this.button6_Click);
			this.newFileToolStripMenuItem.Name = "newFileToolStripMenuItem";
			this.newFileToolStripMenuItem.Size = new Size(126, 22);
			this.newFileToolStripMenuItem.Text = "New file...";
			this.newFileToolStripMenuItem.Click += new EventHandler(this.newFileToolStripMenuItem_Click);
			this.menuStrip1.Items.AddRange(new ToolStripItem[]
			{
				this.fileToolStripMenuItem
			});
			this.menuStrip1.Location = new Point(0, 0);
			this.menuStrip1.Name = "menuStrip1";
			this.menuStrip1.Size = new Size(468, 24);
			this.menuStrip1.TabIndex = 15;
			this.menuStrip1.Text = "menuStrip1";
			this.fileToolStripMenuItem.DropDownItems.AddRange(new ToolStripItem[]
			{
				this.newFileToolStripMenuItem, 
				this.openToolStripMenuItem, 
				this.saveToolStripMenuItem, 
				this.toolStripSeparator1, 
				this.exitToolStripMenuItem
			});
			this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
			this.fileToolStripMenuItem.Size = new Size(37, 20);
			this.fileToolStripMenuItem.Text = "File";
			this.openToolStripMenuItem.Name = "openToolStripMenuItem";
			this.openToolStripMenuItem.Size = new Size(126, 22);
			this.openToolStripMenuItem.Text = "Open";
			this.openToolStripMenuItem.Click += new EventHandler(this.openToolStripMenuItem_Click);
			this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
			this.saveToolStripMenuItem.Size = new Size(126, 22);
			this.saveToolStripMenuItem.Text = "Save";
			this.saveToolStripMenuItem.Click += new EventHandler(this.saveToolStripMenuItem_Click);
			this.toolStripSeparator1.Name = "toolStripSeparator1";
			this.toolStripSeparator1.Size = new Size(123, 6);
			this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
			this.exitToolStripMenuItem.Size = new Size(126, 22);
			this.exitToolStripMenuItem.Text = "Exit";
			this.exitToolStripMenuItem.Click += new EventHandler(this.exitToolStripMenuItem_Click);
			this.button5.Location = new Point(359, 251);
			this.button5.Name = "button5";
			this.button5.Size = new Size(99, 23);
			this.button5.TabIndex = 12;
			this.button5.Text = "Delete";
			this.button5.UseVisualStyleBackColor = true;
			this.button5.Click += new EventHandler(this.button5_Click);
			this.button4.Location = new Point(359, 222);
			this.button4.Name = "button4";
			this.button4.Size = new Size(99, 23);
			this.button4.TabIndex = 11;
			this.button4.Text = "Add Style";
			this.button4.UseVisualStyleBackColor = true;
			this.button4.Click += new EventHandler(this.button4_Click);
			this.button3.Location = new Point(359, 26);
			this.button3.Name = "button3";
			this.button3.Size = new Size(99, 23);
			this.button3.TabIndex = 10;
			this.button3.Text = "Choose";
			this.button3.UseVisualStyleBackColor = true;
			this.button3.Click += new EventHandler(this.button3_Click);
			this.renameToolStripMenuItem.Name = "renameToolStripMenuItem";
			this.renameToolStripMenuItem.Size = new Size(152, 22);
			this.renameToolStripMenuItem.Text = "Rename";
			this.renameToolStripMenuItem.Click += new EventHandler(this.renameToolStripMenuItem_Click);
			this.contextMenuStrip1.Items.AddRange(new ToolStripItem[]
			{
				this.renameToolStripMenuItem
			});
			this.contextMenuStrip1.Name = "contextMenuStrip1";
			this.contextMenuStrip1.Size = new Size(153, 48);
			this.listBox1.ContextMenuStrip = this.contextMenuStrip1;
			this.listBox1.FormattingEnabled = true;
			this.listBox1.Location = new Point(0, 26);
			this.listBox1.Name = "listBox1";
			this.listBox1.Size = new Size(353, 277);
			this.listBox1.TabIndex = 9;
			this.listBox1.DoubleClick += new EventHandler(this.OnDblClick);
			this.openFileDialog1.FileName = "openFileDialog1";
			base.AutoScaleDimensions = new SizeF(6f, 13f);
			base.AutoScaleMode = AutoScaleMode.Font;
			base.ClientSize = new Size(468, 336);
			base.Controls.Add(this.statusStrip1);
			base.Controls.Add(this.button6);
			base.Controls.Add(this.menuStrip1);
			base.Controls.Add(this.button5);
			base.Controls.Add(this.button4);
			base.Controls.Add(this.button3);
			base.Controls.Add(this.listBox1);
			base.Name = "frmAncCircleStyleManager";
			this.Text = "Ancestor Circle Style Manager";
			this.statusStrip1.ResumeLayout(false);
			this.statusStrip1.PerformLayout();
			this.menuStrip1.ResumeLayout(false);
			this.menuStrip1.PerformLayout();
			this.contextMenuStrip1.ResumeLayout(false);
			base.ResumeLayout(false);
			base.PerformLayout();
		}
		public frmAncCircleStyleManager()
		{
			this.InitializeComponent();
			base.StartPosition = FormStartPosition.CenterParent;
		}
		public void FillList()
		{
			this.listBox1.Items.Clear();
			for (clsAncCircleStyle clsAncCircleStyle = (clsAncCircleStyle)this.doc.AncCircleStyles.First; clsAncCircleStyle != null; clsAncCircleStyle = (clsAncCircleStyle)this.doc.AncCircleStyles.Next(clsAncCircleStyle))
			{
				this.listBox1.Items.Add(clsAncCircleStyle.Name);
			}
			this.toolStripStatusLabel1.Text = this.listBox1.Items.Count.ToString();
		}
		private void newFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (this.listBox1.Items.Count > 0)
			{
				DialogResult dialogResult = MessageBox.Show("Are you sure? This will clear all", "Warning", MessageBoxButtons.OKCancel);
				if (dialogResult == DialogResult.Cancel)
				{
					return;
				}
				this.doc.AncCircleStyles.Clear();
				this.FillList();
			}
			this.doc.DescTreeStyleColl.Clear();
		}
		private void SaveStyleFile()
		{
			this.saveFileDialog1.Filter = "Style files (*.sty)|*.sty";
			this.saveFileDialog1.FileName = "";
			if (this.saveFileDialog1.ShowDialog() == DialogResult.OK)
			{
				StreamWriter streamWriter = new StreamWriter(this.saveFileDialog1.FileName, false);
				streamWriter.WriteLine("AncCircleStyles");
				for (clsAncCircleStyle clsAncCircleStyle = (clsAncCircleStyle)this.doc.AncCircleStyles.First; clsAncCircleStyle != null; clsAncCircleStyle = (clsAncCircleStyle)this.doc.AncCircleStyles.Next(clsAncCircleStyle))
				{
					streamWriter.WriteLine(clsAncCircleStyle.Name);
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[0].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[1].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[2].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[3].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[4].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[5].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[6].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[7].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[8].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[9].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.BrushColor[10].ToArgb());
					streamWriter.WriteLine(clsAncCircleStyle.firstname.ToString());
					streamWriter.WriteLine(clsAncCircleStyle.CircularLines.ToString());
				}
				streamWriter.Close();
			}
		}
		private void OpenStyleFile()
		{
			this.openFileDialog1.Filter = "Style files (*.sty)|*.sty";
			this.openFileDialog1.FileName = "";
			if (this.openFileDialog1.ShowDialog() == DialogResult.OK)
			{
				if (!File.Exists(this.openFileDialog1.FileName))
				{
					return;
				}
				StreamReader streamReader = new StreamReader(this.openFileDialog1.FileName);
				string text = streamReader.ReadLine();
				if (text != "AncCircleStyles")
				{
					MessageBox.Show("That is not a AncCircleStylefile!");
					streamReader.Close();
					return;
				}
				this.doc.AncCircleStyles.Clear();
				while (!streamReader.EndOfStream)
				{
					clsAncCircleStyle clsAncCircleStyle = new clsAncCircleStyle();
					text = streamReader.ReadLine();
					clsAncCircleStyle.Name = text;
					text = streamReader.ReadLine();
					int argb;
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[0] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[1] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[2] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[3] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[4] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[5] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[6] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[7] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[8] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[9] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					int.TryParse(text, out argb);
					clsAncCircleStyle.BrushColor[10] = Color.FromArgb(argb);
					text = streamReader.ReadLine();
					if (text == "True")
					{
						clsAncCircleStyle.firstname = true;
					}
					else
					{
						clsAncCircleStyle.firstname = false;
					}
					text = streamReader.ReadLine();
					if (text == "True")
					{
						clsAncCircleStyle.CircularLines = true;
					}
					else
					{
						clsAncCircleStyle.CircularLines = false;
					}
					this.doc.AncCircleStyles.AddLast(clsAncCircleStyle);
				}
				streamReader.Close();
				this.FillList();
			}
		}
		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.OpenStyleFile();
		}
		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			this.SaveStyleFile();
		}
		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.Cancel;
		}
		private clsAncCircleStyle FindSelectedStyle(string nam)
		{
			for (clsAncCircleStyle clsAncCircleStyle = (clsAncCircleStyle)this.doc.AncCircleStyles.First; clsAncCircleStyle != null; clsAncCircleStyle = (clsAncCircleStyle)this.doc.AncCircleStyles.Next(clsAncCircleStyle))
			{
				if (clsAncCircleStyle.Name == nam)
				{
					return clsAncCircleStyle;
				}
			}
			return null;
		}
		private void button3_Click(object sender, EventArgs e)
		{
			if (this.listBox1.SelectedIndex < 0)
			{
				MessageBox.Show("You must select the style you want to use");
				return;
			}
			string nam = this.listBox1.Items[this.listBox1.SelectedIndex].ToString();
			this.choosedstyle = this.FindSelectedStyle(nam);
			if (this.choosedstyle != null)
			{
				this.choosedstyle.CreateBrushes();
				base.DialogResult = DialogResult.OK;
			}
		}
		private void button4_Click(object sender, EventArgs e)
		{
			frmAncCircleStyleEditor frmAncCircleStyleEditor = new frmAncCircleStyleEditor();
			frmAncCircleStyleEditor.doc = this.doc;
			frmAncCircleStyleEditor.SetNew();
			if (frmAncCircleStyleEditor.ShowDialog() == DialogResult.OK)
			{
				this.FillList();
			}
		}
		private void OnDblClick(object sender, EventArgs e)
		{
			if (this.listBox1.SelectedIndex < 0)
			{
				return;
			}
			clsAncCircleStyle clsAncCircleStyle = this.FindSelectedStyle(this.listBox1.Items[this.listBox1.SelectedIndex].ToString());
			if (clsAncCircleStyle != null)
			{
				frmAncCircleStyleEditor frmAncCircleStyleEditor = new frmAncCircleStyleEditor();
				frmAncCircleStyleEditor.doc = this.doc;
				frmAncCircleStyleEditor.SetEdit(clsAncCircleStyle);
				if (frmAncCircleStyleEditor.ShowDialog() == DialogResult.OK)
				{
					this.FillList();
				}
			}
		}
		private void button5_Click(object sender, EventArgs e)
		{
			if (this.listBox1.SelectedIndex > -1)
			{
				string nam = this.listBox1.Items[this.listBox1.SelectedIndex].ToString();
				clsAncCircleStyle clsAncCircleStyle = this.FindSelectedStyle(nam);
				if (clsAncCircleStyle != null)
				{
					this.doc.AncCircleStyles.Delete(clsAncCircleStyle);
					this.FillList();
					return;
				}
			}
			else
			{
				MessageBox.Show("You must select the style you want to delete");
			}
		}
		private bool HasThisName(string name)
		{
			for (clsAncCircleStyle clsAncCircleStyle = (clsAncCircleStyle)this.doc.AncCircleStyles.First; clsAncCircleStyle != null; clsAncCircleStyle = (clsAncCircleStyle)this.doc.AncCircleStyles.Next(clsAncCircleStyle))
			{
				if (clsAncCircleStyle.Name == name)
				{
					return true;
				}
			}
			return false;
		}
		private void button6_Click(object sender, EventArgs e)
		{
			if (this.listBox1.SelectedIndex < 0)
			{
				MessageBox.Show("You must select the style you want to copy");
				return;
			}
			InputBox inputBox = new InputBox("New Style", "Enter name for this style", "", 2);
			if (inputBox.ShowDialog() == DialogResult.OK)
			{
				if (this.HasThisName(inputBox.Value))
				{
					MessageBox.Show("That name already exist, try another name, please");
					return;
				}
				string nam = this.listBox1.Items[this.listBox1.SelectedIndex].ToString();
				clsAncCircleStyle clsAncCircleStyle = this.FindSelectedStyle(nam);
				if (clsAncCircleStyle != null)
				{
					clsAncCircleStyle clsAncCircleStyle2 = new clsAncCircleStyle();
					clsAncCircleStyle2.Name = inputBox.Value;
					clsAncCircleStyle2.BrushColor[0] = clsAncCircleStyle.BrushColor[0];
					clsAncCircleStyle2.BrushColor[1] = clsAncCircleStyle.BrushColor[1];
					clsAncCircleStyle2.BrushColor[2] = clsAncCircleStyle.BrushColor[2];
					clsAncCircleStyle2.BrushColor[3] = clsAncCircleStyle.BrushColor[3];
					clsAncCircleStyle2.BrushColor[4] = clsAncCircleStyle.BrushColor[4];
					clsAncCircleStyle2.BrushColor[5] = clsAncCircleStyle.BrushColor[5];
					clsAncCircleStyle2.BrushColor[6] = clsAncCircleStyle.BrushColor[6];
					clsAncCircleStyle2.BrushColor[7] = clsAncCircleStyle.BrushColor[7];
					clsAncCircleStyle2.BrushColor[8] = clsAncCircleStyle.BrushColor[8];
					clsAncCircleStyle2.BrushColor[9] = clsAncCircleStyle.BrushColor[9];
					clsAncCircleStyle2.BrushColor[10] = clsAncCircleStyle.BrushColor[10];
					clsAncCircleStyle2.CircularLines = clsAncCircleStyle.CircularLines;
					clsAncCircleStyle2.firstname = clsAncCircleStyle.firstname;
					clsAncCircleStyle2.CreateBrushes();
					this.doc.AncCircleStyles.AddLast(clsAncCircleStyle2);
					this.FillList();
				}
			}
		}
		private void renameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (this.listBox1.SelectedIndex > -1)
			{
				string nam = this.listBox1.Items[this.listBox1.SelectedIndex].ToString();
				clsAncCircleStyle clsAncCircleStyle = this.FindSelectedStyle(nam);
				if (clsAncCircleStyle != null)
				{
					InputBox inputBox = new InputBox("Rename style", "Enter name for this style", clsAncCircleStyle.Name, 2);
					if (inputBox.ShowDialog() == DialogResult.OK)
					{
						if (inputBox.Value == clsAncCircleStyle.Name)
						{
							return;
						}
						while (this.HasThisName(inputBox.Value))
						{
							MessageBox.Show("That name already exist, try another name, please");
							if (inputBox.ShowDialog() == DialogResult.Cancel)
							{
								return;
							}
						}
						clsAncCircleStyle.Name = inputBox.Value;
						this.FillList();
					}
				}
			}
		}
	}
}
