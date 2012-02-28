using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKSandbox
{
	public partial class frmAncCircleStyleEditor : Form
	{
		public int mMode;
		private AncestorsCircleOptions style;

		public frmAncCircleStyleEditor(AncestorsCircleOptions style)
		{
			this.InitializeComponent();
			this.style = style;
		}

		private void OnLoad(object sender, EventArgs e)
		{
		}

		public void SetEdit(AncestorsCircleOptions c)
		{
			//this.textBox1.Text = c.Name;
			this.textBox1.Enabled = false;
			this.label1.BackColor = c.BrushColor[0];
			this.label2.BackColor = c.BrushColor[1];
			this.label3.BackColor = c.BrushColor[2];
			this.label4.BackColor = c.BrushColor[3];
			this.label5.BackColor = c.BrushColor[4];
			this.label6.BackColor = c.BrushColor[5];
			this.label7.BackColor = c.BrushColor[6];
			this.label8.BackColor = c.BrushColor[7];
			this.label9.BackColor = c.BrushColor[8];
			this.label10.BackColor = c.BrushColor[9];
			this.label13.BackColor = c.BrushColor[10];

			this.checkBox3.Checked = c.CircularLines;
			this.style = c;
			this.mMode = 1;
		}

		public void SetNew()
		{
			this.label1.BackColor = this.BackColor;
			this.label2.BackColor = this.BackColor;
			this.label3.BackColor = this.BackColor;
			this.label4.BackColor = this.BackColor;
			this.label5.BackColor = this.BackColor;
			this.label6.BackColor = this.BackColor;
			this.label7.BackColor = this.BackColor;
			this.label8.BackColor = this.BackColor;
			this.label9.BackColor = Color.Black;
			this.label10.BackColor = this.BackColor;
			this.label13.BackColor = Color.Black;
			this.checkBox3.Checked = true;
		}

		private void button1_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label1.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label1.BackColor = this.colorDialog1.Color;
			}
		}

		private void button2_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label2.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label2.BackColor = this.colorDialog1.Color;
			}
		}

		private void button3_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label3.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label3.BackColor = this.colorDialog1.Color;
			}
		}

		private void button4_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label4.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label4.BackColor = this.colorDialog1.Color;
			}
		}

		private void button5_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label5.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label5.BackColor = this.colorDialog1.Color;
			}
		}

		private void button6_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label6.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label6.BackColor = this.colorDialog1.Color;
			}
		}

		private void button7_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label7.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label7.BackColor = this.colorDialog1.Color;
			}
		}

		private void button8_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label8.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label8.BackColor = this.colorDialog1.Color;
			}
		}

		private void button9_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label9.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label9.BackColor = this.colorDialog1.Color;
			}
		}

		private void button10_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label10.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label10.BackColor = this.colorDialog1.Color;
			}
		}

		private void button13_Click(object sender, EventArgs e)
		{
			this.colorDialog1.Color = this.label13.BackColor;
			if (this.colorDialog1.ShowDialog() == DialogResult.OK)
			{
				this.label13.BackColor = this.colorDialog1.Color;
			}
		}

		private void button11_Click(object sender, EventArgs e)
		{
			if (this.mMode == 1)
			{
				this.style.BrushColor[0] = this.label1.BackColor;
				this.style.BrushColor[1] = this.label2.BackColor;
				this.style.BrushColor[2] = this.label3.BackColor;
				this.style.BrushColor[3] = this.label4.BackColor;
				this.style.BrushColor[4] = this.label5.BackColor;
				this.style.BrushColor[5] = this.label6.BackColor;
				this.style.BrushColor[6] = this.label7.BackColor;
				this.style.BrushColor[7] = this.label8.BackColor;
				this.style.BrushColor[8] = this.label9.BackColor;
				this.style.BrushColor[9] = this.label10.BackColor;
				this.style.BrushColor[10] = this.label13.BackColor;
				this.style.CreateBrushes();
				this.style.CircularLines = this.checkBox3.Checked;
				base.DialogResult = DialogResult.OK;
				return;
			}

			if (this.textBox1.Text.Length == 0)
			{
				MessageBox.Show("Enter a name for the style, please");
				return;
			}

			AncestorsCircleOptions clsAncCircleStyle = new AncestorsCircleOptions();
			//clsAncCircleStyle.Name = this.textBox1.Text;
			clsAncCircleStyle.BrushColor[0] = this.label1.BackColor;
			clsAncCircleStyle.BrushColor[1] = this.label2.BackColor;
			clsAncCircleStyle.BrushColor[2] = this.label3.BackColor;
			clsAncCircleStyle.BrushColor[3] = this.label4.BackColor;
			clsAncCircleStyle.BrushColor[4] = this.label5.BackColor;
			clsAncCircleStyle.BrushColor[5] = this.label6.BackColor;
			clsAncCircleStyle.BrushColor[6] = this.label7.BackColor;
			clsAncCircleStyle.BrushColor[7] = this.label8.BackColor;
			clsAncCircleStyle.BrushColor[8] = this.label9.BackColor;
			clsAncCircleStyle.BrushColor[9] = this.label10.BackColor;
			clsAncCircleStyle.BrushColor[10] = this.label13.BackColor;
			clsAncCircleStyle.CreateBrushes();
			clsAncCircleStyle.CircularLines = this.checkBox3.Checked;
			//this.doc.AncCircleStyles.AddLast(clsAncCircleStyle);
			base.DialogResult = DialogResult.OK;
		}

		private void button12_Click(object sender, EventArgs e)
		{
			base.DialogResult = DialogResult.Cancel;
		}

	}
}
