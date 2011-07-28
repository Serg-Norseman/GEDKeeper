using GedCom551;
using GKCore;
using GKUI.Charts;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmChart : Form
	{
		private SaveFileDialog SaveDialog1;
		private ToolBar ToolBar1;
		private ToolBarButton tbImageSave;
		private ToolBarButton ToolButton1;
		private ToolBarButton ToolButton2;
		private ToolBarButton tbPrev;
		private ToolBarButton tbNext;
		private ToolBarButton ToolButton3;
		private ContextMenu MenuPerson;
		private MenuItem miEdit;
		private MenuItem N1;
		private MenuItem miSpouseAdd;
		private MenuItem miSonAdd;
		private MenuItem miDaughterAdd;
		private MenuItem miFamilyAdd;
		private ToolBarButton ToolButton5;
		private MenuItem N2;
		private MenuItem miDelete;
		private MenuItem N3;
		private MenuItem miRebuildKinships;
		private ToolBarButton tbFilter;
		private ToolBarButton ToolButton6;
		private ToolBarButton tbModes;
		private ContextMenu MenuModes;
		private MenuItem miModeBoth;
		private MenuItem miModeAncestors;
		private MenuItem miModeDescendants;
		private MenuItem N7;
		private MenuItem miTraceRoot;
		private MenuItem miRebuildTree;
		private ToolBarButton tbScales;
		private ContextMenu MenuScales;
		private MenuItem N501;
		private MenuItem N601;
		private MenuItem N701;
		private MenuItem N801;
		private MenuItem N901;
		private MenuItem N1001;
		private ToolBarButton tbGens;
		private ContextMenu MenuGens;
		private MenuItem miGensInf;
		private MenuItem miGens1;
		private MenuItem miGens2;
		private MenuItem miGens3;
		private MenuItem miGens4;
		private MenuItem miGens5;
		private MenuItem miGens6;
		private MenuItem miGens7;
		private MenuItem miGens8;
		private MenuItem miGens9;
		private TBackManager FBackman;
		private TfmBase FBase;
		private TAncestryChartBox.TChartKind FChartKind;
		private bool FDown;
		private string FFileName;
		private int FGensLimit;
		private TGEDCOMIndividualRecord FPerson;
		private int FScale;
		private TGEDCOMTree FTree;
		private TAncestryChartBox FTreeBox;
		private int FX;
		private int FY;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
			set
			{
				this.FBase = value;
			}
		}

		public TAncestryChartBox.TChartKind ChartKind
		{
			get { return this.FChartKind; }
			set { this.SetChartKind(value); }
		}

		[Browsable(false)]
		public string FileName
		{
			get
			{
				return this.FFileName;
			}
			set
			{
				this.FFileName = value;
			}
		}
		[Browsable(false)]
		public TGEDCOMIndividualRecord Person
		{
			get
			{
				return this.FPerson;
			}
			set
			{
				this.FPerson = value;
			}
		}
		[Browsable(false)]
		public TGEDCOMTree Tree
		{
			get
			{
				return this.FTree;
			}
			set
			{
				this.FTree = value;
			}
		}
		private void DoFilter()
		{
			TfmTreeFilter fmTreeFilter = new TfmTreeFilter(this.Base);
			try
			{
				fmTreeFilter.Filter = this.FTreeBox.Filter;
				if (fmTreeFilter.ShowDialog() == DialogResult.OK)
				{
					this.GenChart(true);
				}
			}
			finally
			{
				TObjectHelper.Free(fmTreeFilter);
			}
		}
		private void DoImageSave()
		{
			if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
			{
				this.FTreeBox.SaveSnapshot(this.SaveDialog1.FileName);
			}
		}
		private void DoNext()
		{
			this.FBackman.BeginNav();
			try
			{
				this.FPerson = (this.FBackman.Next() as TGEDCOMIndividualRecord);
				this.GenChart(true);
				this.NavRefresh();
			}
			finally
			{
				this.FBackman.EndNav();
			}
		}
		private void DoPrev()
		{
			this.FBackman.BeginNav();
			try
			{
				this.FPerson = (this.FBackman.Back() as TGEDCOMIndividualRecord);
				this.GenChart(true);
				this.NavRefresh();
			}
			finally
			{
				this.FBackman.EndNav();
			}
		}
		private void InternalChildAdd(TGEDCOMObject.TGEDCOMSex aNeedSex)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				if (i_rec.SpouseToFamilyLinksCount == 0)
				{
					TGKSys.ShowError(GKL.LSList[211]);
				}
				else
				{
					if (i_rec.SpouseToFamilyLinksCount > 1)
					{
						TGKSys.ShowError("У данной персоны несколько семей. Выбор еще не реализован.");
					}
					else
					{
						TGEDCOMFamilyRecord fam = i_rec.GetSpouseToFamilyLink(0).Family;
						TGEDCOMIndividualRecord i_child = this.Base.SelectPerson(fam.Husband.Value as TGEDCOMIndividualRecord, TGenEngine.TTargetMode.tmAncestor, aNeedSex);
						if (i_child != null && this.Base.Engine.AddFamilyChild(fam, i_child))
						{
							this.UpdateChart();
						}
					}
				}
			}
		}
		private void NavRefresh()
		{
			this.tbPrev.Enabled = this.FBackman.CanBackward();
			this.tbNext.Enabled = this.FBackman.CanForward();
		}
		private void NavAdd(TGEDCOMIndividualRecord aRec)
		{
			if (aRec != null && !this.FBackman.Busy)
			{
				this.FBackman.Current = aRec;
				this.NavRefresh();
			}
		}
		private void UpdateChart()
		{
			if (this.FBase != null)
			{
				this.FBase.ListsRefresh(false);
			}
			this.GenChart(true);
		}

		private void SetChartKind([In] TAncestryChartBox.TChartKind Value)
		{
			this.FChartKind = Value;
			TAncestryChartBox.TChartKind fChartKind = this.FChartKind;
			if (fChartKind != TAncestryChartBox.TChartKind.ckAncestors)
			{
				if (fChartKind != TAncestryChartBox.TChartKind.ckDescendants)
				{
					if (fChartKind == TAncestryChartBox.TChartKind.ckBoth)
					{
						this.miModeBoth.Checked = true;
					}
				}
				else
				{
					this.miModeDescendants.Checked = true;
				}
			}
			else
			{
				this.miModeAncestors.Checked = true;
			}
		}
		private void InitializeComponent()
		{
			this.SaveDialog1 = new SaveFileDialog();
			this.ToolBar1 = new ToolBar();
			this.tbImageSave = new ToolBarButton();
			this.ToolButton1 = new ToolBarButton();
			this.tbGens = new ToolBarButton();
			this.MenuGens = new ContextMenu();
			this.miGensInf = new MenuItem();
			this.miGens1 = new MenuItem();
			this.miGens2 = new MenuItem();
			this.miGens3 = new MenuItem();
			this.miGens4 = new MenuItem();
			this.miGens5 = new MenuItem();
			this.miGens6 = new MenuItem();
			this.miGens7 = new MenuItem();
			this.miGens8 = new MenuItem();
			this.miGens9 = new MenuItem();
			this.ToolButton2 = new ToolBarButton();
			this.tbPrev = new ToolBarButton();
			this.tbNext = new ToolBarButton();
			this.ToolButton3 = new ToolBarButton();
			this.tbScales = new ToolBarButton();
			this.MenuScales = new ContextMenu();
			this.N501 = new MenuItem();
			this.N601 = new MenuItem();
			this.N701 = new MenuItem();
			this.N801 = new MenuItem();
			this.N901 = new MenuItem();
			this.N1001 = new MenuItem();
			this.ToolButton5 = new ToolBarButton();
			this.tbFilter = new ToolBarButton();
			this.ToolButton6 = new ToolBarButton();
			this.tbModes = new ToolBarButton();
			this.MenuModes = new ContextMenu();
			this.miModeBoth = new MenuItem();
			this.miModeAncestors = new MenuItem();
			this.miModeDescendants = new MenuItem();
			this.N7 = new MenuItem();
			this.miTraceRoot = new MenuItem();
			this.MenuPerson = new ContextMenu();
			this.miEdit = new MenuItem();
			this.N1 = new MenuItem();
			this.miFamilyAdd = new MenuItem();
			this.miSpouseAdd = new MenuItem();
			this.miSonAdd = new MenuItem();
			this.miDaughterAdd = new MenuItem();
			this.N2 = new MenuItem();
			this.miDelete = new MenuItem();
			this.N3 = new MenuItem();
			this.miRebuildTree = new MenuItem();
			this.miRebuildKinships = new MenuItem();
			base.SuspendLayout();
			this.SaveDialog1.DefaultExt = "tga";
			this.SaveDialog1.Filter = "Файлы BMP (*.bmp)|*.bmp|Файлы JPEG (*.jpg)|*.jpg|Файлы EMF (*.emf)|*.emf";
			this.SaveDialog1.FilterIndex = 2;
			this.ToolBar1.Appearance = ToolBarAppearance.Flat;
			ToolBar.ToolBarButtonCollection arg_311_0 = this.ToolBar1.Buttons;
			ToolBarButton[] array = null;
			ToolBarButton[] array2 = array;
			ToolBarButton[] array3;
			ToolBarButton[] expr_27B = array3 = new ToolBarButton[12];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 12)
				{
					num = 12;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_27B;
			array[0] = this.tbImageSave;
			array[1] = this.ToolButton1;
			array[2] = this.tbGens;
			array[3] = this.ToolButton2;
			array[4] = this.tbPrev;
			array[5] = this.tbNext;
			array[6] = this.ToolButton3;
			array[7] = this.tbScales;
			array[8] = this.ToolButton5;
			array[9] = this.tbFilter;
			array[10] = this.ToolButton6;
			array[11] = this.tbModes;
			arg_311_0.AddRange(array);
			this.ToolBar1.DropDownArrows = true;
			this.ToolBar1.Location = new Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.ShowToolTips = true;
			this.ToolBar1.Size = new Size(822, 28);
			this.ToolBar1.TabIndex = 0;
			this.ToolBar1.ButtonClick += new ToolBarButtonClickEventHandler(this.ToolBar1_ButtonClick);
			this.tbImageSave.ImageIndex = 6;
			this.tbImageSave.ToolTipText = "Сохранить изображение";
			this.ToolButton1.ImageIndex = 7;
			this.ToolButton1.Style = ToolBarButtonStyle.Separator;
			this.tbGens.DropDownMenu = this.MenuGens;
			this.tbGens.ImageIndex = 22;
			this.tbGens.Style = ToolBarButtonStyle.DropDownButton;
			Menu.MenuItemCollection arg_481_0 = this.MenuGens.MenuItems;
			MenuItem[] array4 = null;
			MenuItem[] array5 = array4;
			MenuItem[] array6;
			MenuItem[] expr_3FF = array6 = new MenuItem[10];
			if (array5 != null)
			{
				int num2;
				if ((num2 = array5.Length) > 10)
				{
					num2 = 10;
				}
				if (num2 > 0)
				{
					Array.Copy(array5, array6, num2);
				}
			}
			array4 = expr_3FF;
			array4[0] = this.miGensInf;
			array4[1] = this.miGens1;
			array4[2] = this.miGens2;
			array4[3] = this.miGens3;
			array4[4] = this.miGens4;
			array4[5] = this.miGens5;
			array4[6] = this.miGens6;
			array4[7] = this.miGens7;
			array4[8] = this.miGens8;
			array4[9] = this.miGens9;
			arg_481_0.AddRange(array4);
			this.miGensInf.Checked = true;
			this.miGensInf.Index = 0;
			this.miGensInf.Text = "Inf";
			this.miGensInf.Click += new EventHandler(this.miGens9Click);
			this.miGens1.Index = 1;
			this.miGens1.Text = "1";
			this.miGens1.Click += new EventHandler(this.miGens9Click);
			this.miGens2.Index = 2;
			this.miGens2.Text = "2";
			this.miGens2.Click += new EventHandler(this.miGens9Click);
			this.miGens3.Index = 3;
			this.miGens3.Text = "3";
			this.miGens3.Click += new EventHandler(this.miGens9Click);
			this.miGens4.Index = 4;
			this.miGens4.Text = "4";
			this.miGens4.Click += new EventHandler(this.miGens9Click);
			this.miGens5.Index = 5;
			this.miGens5.Text = "5";
			this.miGens5.Click += new EventHandler(this.miGens9Click);
			this.miGens6.Index = 6;
			this.miGens6.Text = "6";
			this.miGens6.Click += new EventHandler(this.miGens9Click);
			this.miGens7.Index = 7;
			this.miGens7.Text = "7";
			this.miGens7.Click += new EventHandler(this.miGens9Click);
			this.miGens8.Index = 8;
			this.miGens8.Text = "8";
			this.miGens8.Click += new EventHandler(this.miGens9Click);
			this.miGens9.Index = 9;
			this.miGens9.Text = "9";
			this.miGens9.Click += new EventHandler(this.miGens9Click);
			this.ToolButton2.ImageIndex = 8;
			this.ToolButton2.Style = ToolBarButtonStyle.Separator;
			this.tbPrev.Enabled = false;
			this.tbPrev.ImageIndex = 22;
			this.tbNext.Enabled = false;
			this.tbNext.ImageIndex = 23;
			this.ToolButton3.ImageIndex = 24;
			this.ToolButton3.Style = ToolBarButtonStyle.Separator;
			this.tbScales.DropDownMenu = this.MenuScales;
			this.tbScales.ImageIndex = 22;
			this.tbScales.Style = ToolBarButtonStyle.DropDownButton;
			Menu.MenuItemCollection arg_78F_0 = this.MenuScales.MenuItems;
			MenuItem[] array7 = null;
			MenuItem[] array8 = array7;
			MenuItem[] array9;
			MenuItem[] expr_734 = array9 = new MenuItem[6];
			if (array8 != null)
			{
				int num3;
				if ((num3 = array8.Length) > 6)
				{
					num3 = 6;
				}
				if (num3 > 0)
				{
					Array.Copy(array8, array9, num3);
				}
			}
			array7 = expr_734;
			array7[0] = this.N501;
			array7[1] = this.N601;
			array7[2] = this.N701;
			array7[3] = this.N801;
			array7[4] = this.N901;
			array7[5] = this.N1001;
			arg_78F_0.AddRange(array7);
			this.N501.Index = 0;
			this.N501.Text = "  50%";
			this.N501.Click += new EventHandler(this.N1001Click);
			this.N601.Index = 1;
			this.N601.Text = "  60%";
			this.N601.Click += new EventHandler(this.N1001Click);
			this.N701.Index = 2;
			this.N701.Text = "  70%";
			this.N701.Click += new EventHandler(this.N1001Click);
			this.N801.Index = 3;
			this.N801.Text = "  80%";
			this.N801.Click += new EventHandler(this.N1001Click);
			this.N901.Index = 4;
			this.N901.Text = "  90%";
			this.N901.Click += new EventHandler(this.N1001Click);
			this.N1001.Checked = true;
			this.N1001.Index = 5;
			this.N1001.Text = "100%";
			this.N1001.Click += new EventHandler(this.N1001Click);
			this.ToolButton5.ImageIndex = 25;
			this.ToolButton5.Style = ToolBarButtonStyle.Separator;
			this.tbFilter.ImageIndex = 16;
			this.tbFilter.ToolTipText = "Фильтрация древа";
			this.ToolButton6.ImageIndex = 17;
			this.ToolButton6.Style = ToolBarButtonStyle.Separator;
			this.tbModes.DropDownMenu = this.MenuModes;
			this.tbModes.ImageIndex = 21;
			this.tbModes.Style = ToolBarButtonStyle.DropDownButton;
			this.tbModes.ToolTipText = "Режимы";
			Menu.MenuItemCollection arg_9CC_0 = this.MenuModes.MenuItems;
			MenuItem[] array10 = null;
			MenuItem[] array11 = array10;
			MenuItem[] array12;
			MenuItem[] expr_973 = array12 = new MenuItem[5];
			if (array11 != null)
			{
				int num4;
				if ((num4 = array11.Length) > 5)
				{
					num4 = 5;
				}
				if (num4 > 0)
				{
					Array.Copy(array11, array12, num4);
				}
			}
			array10 = expr_973;
			array10[0] = this.miModeBoth;
			array10[1] = this.miModeAncestors;
			array10[2] = this.miModeDescendants;
			array10[3] = this.N7;
			array10[4] = this.miTraceRoot;
			arg_9CC_0.AddRange(array10);
			this.miModeBoth.Checked = true;
			this.miModeBoth.Index = 0;
			this.miModeBoth.Text = "miModeBoth";
			this.miModeBoth.Click += new EventHandler(this.miModeDescendantsClick);
			this.miModeAncestors.Index = 1;
			this.miModeAncestors.Text = "miModeAncestors";
			this.miModeAncestors.Click += new EventHandler(this.miModeDescendantsClick);
			this.miModeDescendants.Index = 2;
			this.miModeDescendants.Text = "miModeDescendants";
			this.miModeDescendants.Click += new EventHandler(this.miModeDescendantsClick);
			this.N7.Index = 3;
			this.N7.Text = "-";
			this.miTraceRoot.Index = 4;
			this.miTraceRoot.Text = "miTraceRoot";
			this.miTraceRoot.Click += new EventHandler(this.miTraceRootClick);
			Menu.MenuItemCollection arg_B68_0 = this.MenuPerson.MenuItems;
			MenuItem[] array13 = null;
			MenuItem[] array14 = array13;
			MenuItem[] array15;
			MenuItem[] expr_ADC = array15 = new MenuItem[11];
			if (array14 != null)
			{
				int num5;
				if ((num5 = array14.Length) > 11)
				{
					num5 = 11;
				}
				if (num5 > 0)
				{
					Array.Copy(array14, array15, num5);
				}
			}
			array13 = expr_ADC;
			array13[0] = this.miEdit;
			array13[1] = this.N1;
			array13[2] = this.miFamilyAdd;
			array13[3] = this.miSpouseAdd;
			array13[4] = this.miSonAdd;
			array13[5] = this.miDaughterAdd;
			array13[6] = this.N2;
			array13[7] = this.miDelete;
			array13[8] = this.N3;
			array13[9] = this.miRebuildTree;
			array13[10] = this.miRebuildKinships;
			arg_B68_0.AddRange(array13);
			this.miEdit.Index = 0;
			this.miEdit.Text = "miEdit";
			this.miEdit.Click += new EventHandler(this.miEditClick);
			this.N1.Index = 1;
			this.N1.Text = "-";
			this.miFamilyAdd.Index = 2;
			this.miFamilyAdd.Text = "miFamilyAdd";
			this.miFamilyAdd.Click += new EventHandler(this.miFamilyAddClick);
			this.miSpouseAdd.Index = 3;
			this.miSpouseAdd.Text = "miSpouseAdd";
			this.miSpouseAdd.Click += new EventHandler(this.miSpouseAddClick);
			this.miSonAdd.Index = 4;
			this.miSonAdd.Text = "miSonAdd";
			this.miSonAdd.Click += new EventHandler(this.miSonAddClick);
			this.miDaughterAdd.Index = 5;
			this.miDaughterAdd.Text = "miDaughterAdd";
			this.miDaughterAdd.Click += new EventHandler(this.miDaughterAddClick);
			this.N2.Index = 6;
			this.N2.Text = "-";
			this.miDelete.Index = 7;
			this.miDelete.Text = "miDelete";
			this.miDelete.Click += new EventHandler(this.miDeleteClick);
			this.N3.Index = 8;
			this.N3.Text = "-";
			this.miRebuildTree.Index = 9;
			this.miRebuildTree.Text = "miRebuildTree";
			this.miRebuildTree.Click += new EventHandler(this.miRebuildTreeClick);
			this.miRebuildKinships.Index = 10;
			this.miRebuildKinships.Text = "miRebuildKinships";
			this.miRebuildKinships.Click += new EventHandler(this.miRebuildKinshipsClick);
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(822, 453);
			base.Controls.Add(this.ToolBar1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.KeyPreview = true;
			base.Name = "TfmChart";
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Диаграмма";
			base.KeyDown += new KeyEventHandler(this.TfmChart_KeyDown);
			base.ResumeLayout(false);
		}
		private void TfmChart_KeyDown(object sender, KeyEventArgs e)
		{
			Keys keyCode = e.KeyCode;
			if (keyCode != Keys.Escape)
			{
				if (keyCode != Keys.F6)
				{
					if (keyCode == Keys.F7)
					{
						this.FTreeBox.RebuildKinships();
					}
				}
				else
				{
					this.miRebuildTreeClick(null, null);
				}
			}
			else
			{
				base.Close();
			}
		}
		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (object.Equals(e.Button, this.tbImageSave))
			{
				this.DoImageSave();
			}
			else
			{
				if (object.Equals(e.Button, this.tbPrev))
				{
					this.DoPrev();
				}
				else
				{
					if (object.Equals(e.Button, this.tbNext))
					{
						this.DoNext();
					}
					else
					{
						if (object.Equals(e.Button, this.tbFilter))
						{
							this.DoFilter();
						}
					}
				}
			}
		}
		private void ImageTree_MouseDown(object sender, MouseEventArgs e)
		{
			this.FX = e.X;
			this.FY = e.Y;
			if (e.Button == MouseButtons.Right)
			{
				this.FTreeBox.Cursor = Cursors.SizeAll;
				this.FDown = true;
			}
		}
		private void ImageTree_MouseMove(object sender, MouseEventArgs e)
		{
			if (this.FDown)
			{
				this.FTreeBox.LeftPos = this.FTreeBox.LeftPos - (e.X - this.FX);
				this.FTreeBox.TopPos = this.FTreeBox.TopPos - (e.Y - this.FY);
				this.FX = e.X;
				this.FY = e.Y;
			}
		}
		private void ImageTree_MouseUp(object sender, MouseEventArgs e)
		{
			if (this.FDown)
			{
				this.FTreeBox.Cursor = Cursors.Default;
				this.FDown = false;
			}
			this.FTreeBox.SelectBy(e.X, e.Y);
			if (this.FTreeBox.Selected != null && this.FTreeBox.Selected.Rec != null)
			{
				MouseButtons button = e.Button;
				if (button != MouseButtons.Left)
				{
					if (button == MouseButtons.Right)
					{
						this.MenuPerson.Show(this.FTreeBox, new Point(e.X, e.Y));
					}
				}
				else
				{
					if (this.miTraceRoot.Checked)
					{
						this.Person = this.FTreeBox.Selected.Rec;
						this.GenChart(true);
						this.FTreeBox.SelectByRec(this.FPerson);
					}
				}
			}
		}
		private void ImageTree_DblClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				if (this.FBase.ModifyPerson(ref i_rec))
				{
					this.UpdateChart();
				}
			}
		}
		private void miGens9Click(object sender, EventArgs e)
		{
			this.miGensInf.Checked = false;
			this.miGens1.Checked = false;
			this.miGens2.Checked = false;
			this.miGens3.Checked = false;
			this.miGens4.Checked = false;
			this.miGens5.Checked = false;
			this.miGens6.Checked = false;
			this.miGens7.Checked = false;
			this.miGens8.Checked = false;
			this.miGens9.Checked = false;
			(sender as MenuItem).Checked = true;
			if (object.Equals(sender, this.miGensInf))
			{
				this.FGensLimit = -1;
			}
			if (object.Equals(sender, this.miGens1))
			{
				this.FGensLimit = 1;
			}
			if (object.Equals(sender, this.miGens2))
			{
				this.FGensLimit = 2;
			}
			if (object.Equals(sender, this.miGens3))
			{
				this.FGensLimit = 3;
			}
			if (object.Equals(sender, this.miGens4))
			{
				this.FGensLimit = 4;
			}
			if (object.Equals(sender, this.miGens5))
			{
				this.FGensLimit = 5;
			}
			if (object.Equals(sender, this.miGens6))
			{
				this.FGensLimit = 6;
			}
			if (object.Equals(sender, this.miGens7))
			{
				this.FGensLimit = 7;
			}
			if (object.Equals(sender, this.miGens8))
			{
				this.FGensLimit = 8;
			}
			if (object.Equals(sender, this.miGens9))
			{
				this.FGensLimit = 9;
			}
			this.GenChart(true);
		}
		private void N1001Click(object sender, EventArgs e)
		{
			this.N501.Checked = false;
			this.N601.Checked = false;
			this.N701.Checked = false;
			this.N801.Checked = false;
			this.N901.Checked = false;
			this.N1001.Checked = false;
			(sender as MenuItem).Checked = true;
			if (object.Equals(sender, this.N501))
			{
				this.FScale = 5;
			}
			if (object.Equals(sender, this.N601))
			{
				this.FScale = 6;
			}
			if (object.Equals(sender, this.N701))
			{
				this.FScale = 7;
			}
			if (object.Equals(sender, this.N801))
			{
				this.FScale = 8;
			}
			if (object.Equals(sender, this.N901))
			{
				this.FScale = 9;
			}
			if (object.Equals(sender, this.N1001))
			{
				this.FScale = 10;
			}
			this.GenChart(true);
		}
		private void miEditClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				if (this.FBase.ModifyPerson(ref i_rec))
				{
					this.UpdateChart();
				}
			}
		}
		private void miSpouseAddClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMIndividualRecord i_rec = p.Rec;
				TGEDCOMObject.TGEDCOMSex sex = i_rec.Sex;
				TGEDCOMObject.TGEDCOMSex sx;
				if (sex != TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (sex != TGEDCOMObject.TGEDCOMSex.svFemale)
					{
						TGKSys.ShowError(GKL.LSList[210]);
						return;
					}
					sx = TGEDCOMObject.TGEDCOMSex.svMale;
				}
				else
				{
					sx = TGEDCOMObject.TGEDCOMSex.svFemale;
				}
				TGEDCOMIndividualRecord i_spouse = this.Base.SelectPerson(null, TGenEngine.TTargetMode.tmNone, sx);
				if (i_spouse != null)
				{
					TGEDCOMFamilyRecord fam = TGenEngine.CreateFamilyEx(this.FTree);
					this.Base.Engine.AddFamilySpouse(fam, i_rec);
					this.Base.Engine.AddFamilySpouse(fam, i_spouse);
					this.UpdateChart();
				}
			}
		}
		private void miSonAddClick(object sender, EventArgs e)
		{
			this.InternalChildAdd(TGEDCOMObject.TGEDCOMSex.svMale);
		}
		private void miDaughterAddClick(object sender, EventArgs e)
		{
			this.InternalChildAdd(TGEDCOMObject.TGEDCOMSex.svFemale);
		}
		private void miFamilyAddClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				TGEDCOMObject.TGEDCOMSex sex = p.Rec.Sex;
				if (sex < TGEDCOMObject.TGEDCOMSex.svMale || sex >= TGEDCOMObject.TGEDCOMSex.svUndetermined)
				{
					TGKSys.ShowError(GKL.LSList[210]);
				}
				else
				{
					TGEDCOMFamilyRecord fam = TGenEngine.CreateFamilyEx(this.FTree);
					this.Base.Engine.AddFamilySpouse(fam, p.Rec);
					this.UpdateChart();
				}
			}
		}
		private void miDeleteClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null && !object.Equals(p, this.FTreeBox.Root))
			{
				this.FBase.DeleteIndividualRecord(p.Rec, true);
				this.GenChart(true);
				this.NavRefresh();
			}
		}
		private void miRebuildKinshipsClick(object sender, EventArgs e)
		{
			this.FTreeBox.RebuildKinships();
		}

		private void miTraceRootClick(object sender, EventArgs e)
		{
			this.miTraceRoot.Checked = !this.miTraceRoot.Checked;
		}

		private void miModeDescendantsClick(object sender, EventArgs e)
		{
			if (!object.Equals(this.miModeBoth, sender))
			{
				this.miModeBoth.Checked = false;
			}
			if (!object.Equals(this.miModeAncestors, sender))
			{
				this.miModeAncestors.Checked = false;
			}
			if (!object.Equals(this.miModeDescendants, sender))
			{
				this.miModeDescendants.Checked = false;
			}
			((MenuItem)sender).Checked = true;

			TAncestryChartBox.TChartKind newMode = TAncestryChartBox.TChartKind.ckBoth;

			if (this.miModeBoth.Checked)
			{
				newMode = TAncestryChartBox.TChartKind.ckBoth;
			}
			else
			{
				if (this.miModeAncestors.Checked)
				{
					newMode = TAncestryChartBox.TChartKind.ckAncestors;
				}
				else
				{
					if (this.miModeDescendants.Checked)
					{
						newMode = TAncestryChartBox.TChartKind.ckDescendants;
					}
				}
			}
			if (this.FChartKind != newMode)
			{
				this.FChartKind = newMode;
				this.GenChart(true);
			}
		}

		private void miRebuildTreeClick(object sender, EventArgs e)
		{
			TPerson p = this.FTreeBox.Selected;
			if (p != null && p.Rec != null)
			{
				this.FPerson = p.Rec;
				this.GenChart(true);
				this.NavRefresh();
			}
		}
		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FBackman.Free();
			}
			base.Dispose(Disposing);
		}
		public TfmChart(TfmBase aBase)
		{
			this.InitializeComponent();
			base.MdiParent = GKL.fmGEDKeeper;
			this.FBase = aBase;
			this.ToolBar1.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
			this.FTreeBox = new TAncestryChartBox();
			this.FTreeBox.Dock = DockStyle.Fill;
			this.FTreeBox.MouseDown += new MouseEventHandler(this.ImageTree_MouseDown);
			this.FTreeBox.MouseUp += new MouseEventHandler(this.ImageTree_MouseUp);
			this.FTreeBox.MouseMove += new MouseEventHandler(this.ImageTree_MouseMove);
			this.FTreeBox.DoubleClick += new EventHandler(this.ImageTree_DblClick);
			base.Controls.Add(this.FTreeBox);
			base.Controls.SetChildIndex(this.FTreeBox, 0);
			base.Controls.SetChildIndex(this.ToolBar1, 1);
			this.FBackman = new TBackManager();
			this.NavRefresh();
			this.FGensLimit = -1;
			this.FScale = 10;
			this.SetLang();
		}

		public static bool CheckData(TGEDCOMTree aTree, TGEDCOMIndividualRecord iRec, TAncestryChartBox.TChartKind aKind)
		{
			bool Result = true;
			if (aKind == TAncestryChartBox.TChartKind.ckAncestors || aKind == TAncestryChartBox.TChartKind.ckBoth)
			{
				TGenEngine.InitExtCounts(aTree, -1);
				int anc_count = TGenEngine.GetAncestorsCount(iRec);
				if (anc_count > 2048)
				{
					TGKSys.ShowMessage(string.Format(GKL.LSList[212], new object[]
					{
						anc_count.ToString()
					}));
					Result = false;
					return Result;
				}
			}
			if (aKind >= TAncestryChartBox.TChartKind.ckDescendants && aKind < (TAncestryChartBox.TChartKind)3)
			{
				TGenEngine.InitExtCounts(aTree, -1);
				int desc_count = TGenEngine.GetDescendantsCount(iRec);
				if (desc_count > 2048)
				{
					TGKSys.ShowMessage(string.Format(GKL.LSList[213], new object[]
					{
						desc_count.ToString()
					}));
					Result = false;
				}
			}
			return Result;
		}
		public void GenChart(bool aShow)
		{
			if (this.FPerson == null)
			{
				TGKSys.ShowError(GKL.LSList[209]);
			}
			else
			{
				try
				{
					this.NavAdd(this.FPerson);
					this.FTreeBox.DepthLimit = this.FGensLimit;
					this.FTreeBox.Options = GKL.fmGEDKeeper.Options.ChartOptions;
					this.FTreeBox.Engine = this.Base.Engine;
					this.FTreeBox.Tree = this.FTree;
					this.FTreeBox.ShieldState = this.Base.ShieldState;
					this.FTreeBox.Scale = this.FScale * 10;

					this.FTreeBox.GenChart(this.FPerson, this.FChartKind);

					TAncestryChartBox.TChartKind fChartKind = this.FChartKind;

					if (fChartKind != TAncestryChartBox.TChartKind.ckAncestors)
					{
						if (fChartKind != TAncestryChartBox.TChartKind.ckDescendants)
						{
							if (fChartKind == TAncestryChartBox.TChartKind.ckBoth)
							{
								this.Text = GKL.LSList[25];
							}
						}
						else
						{
							this.Text = GKL.LSList[24];
						}
					}
					else
					{
						this.Text = GKL.LSList[23];
					}
					this.Text = this.Text + " \"" + this.FFileName + "\"";
					if (aShow)
					{
						base.Show();
					}
				}
				catch (Exception E)
				{
					TGKSys.ShowError(E.Message);
				}
			}
		}

		public void SetLang()
		{
			this.miGensInf.Text = GKL.LSList[215];
			this.miGensInf.Checked = true;
			this.miModeBoth.Text = GKL.LSList[222];
			this.miModeAncestors.Text = GKL.LSList[223];
			this.miModeDescendants.Text = GKL.LSList[224];
			this.miTraceRoot.Text = GKL.LSList[225];
			this.miEdit.Text = GKL.LSList[226];
			this.miFamilyAdd.Text = GKL.LSList[227];
			this.miSpouseAdd.Text = GKL.LSList[228];
			this.miSonAdd.Text = GKL.LSList[229];
			this.miDaughterAdd.Text = GKL.LSList[230];
			this.miDelete.Text = GKL.LSList[231];
			this.miRebuildTree.Text = GKL.LSList[232];
			this.miRebuildKinships.Text = GKL.LSList[233];
		}
	}
}
