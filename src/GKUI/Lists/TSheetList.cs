using System;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI.Lists
{
	public class TSheetList : ContainerControl, IDisposable
	{
		public delegate void TModifyEvent(object Sender, object ItemData, TGenEngine.TRecAction Action);

		public enum TListButton : byte
		{
			lbAdd,
			lbEdit,
			lbDelete,
			lbJump,
			lbMoveUp,
			lbMoveDown
		}

		private ToolBarButton FBtnAdd;
		private ToolBarButton FBtnDelete;
		private ToolBarButton FBtnEdit;
		private ToolBarButton FBtnLinkJump;
		private ToolBarButton FBtnMoveUp;
		private ToolBarButton FBtnMoveDown;
		private ToolBar FToolBar;
		private TEnumSet FButtons;
		private TGKListView FList;
		private TSheetList.TModifyEvent FOnModify;
		private bool FReadOnly;

		public event TSheetList.TModifyEvent OnModify
		{
			add
			{
				this.FOnModify = value;
			}
			remove
			{
				if (this.FOnModify == value)
				{
					this.FOnModify = null;
				}
			}
		}

		public TEnumSet Buttons
		{
			get { return this.FButtons; }
			set { this.SetButtons(value); }
		}

		public TGKListView List
		{
			get { return this.FList; }
		}

		public ToolBar ToolBar
		{
			get { return this.FToolBar; }
		}

		public bool ReadOnly
		{
			get { return this.FReadOnly; }
			set { this.SetReadOnly(value); }
		}

		private void ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (object.Equals(e.Button, this.FBtnAdd))
			{
				this.ItemAdd();
			}
			else if (object.Equals(e.Button, this.FBtnEdit))
			{
				this.ItemEdit();
			}
			else if (object.Equals(e.Button, this.FBtnDelete))
			{
				this.ItemDelete();
			}
			else if (object.Equals(e.Button, this.FBtnLinkJump))
			{
				this.ItemJump();
			}
			else if (object.Equals(e.Button, this.FBtnMoveUp))
			{
				this.ItemMoveUp();
			}
			else if (object.Equals(e.Button, this.FBtnMoveDown))
			{
				this.ItemMoveDown();
			}
		}

		private void List_DoubleClick(object sender, EventArgs e)
		{
			this.ItemEdit();
		}

		private void List_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.Control)
			{
				Keys keyCode = e.KeyCode;
				if (keyCode != Keys.Return)
				{
					if (keyCode != Keys.D)
					{
						if (keyCode == Keys.I)
						{
							this.ItemAdd();
						}
					}
					else
					{
						this.ItemDelete();
					}
				}
				else
				{
					this.ItemEdit();
				}
			}
		}

		private void SheetShow(object sender, EventArgs e)
		{
			this.FList.Focus();
		}

		private void SetButtons([In] TEnumSet Value)
		{
			this.FButtons = Value;
			this.FBtnAdd.Visible = this.FButtons.InSet(TSheetList.TListButton.lbAdd);
			this.FBtnDelete.Visible = this.FButtons.InSet(TSheetList.TListButton.lbDelete);
			this.FBtnEdit.Visible = this.FButtons.InSet(TSheetList.TListButton.lbEdit);
			this.FBtnLinkJump.Visible = this.FButtons.InSet(TSheetList.TListButton.lbJump);
			this.FBtnMoveUp.Visible = this.FButtons.InSet(TSheetList.TListButton.lbMoveUp);
			this.FBtnMoveDown.Visible = this.FButtons.InSet(TSheetList.TListButton.lbMoveDown);
			this.FToolBar.Visible = !this.FButtons.IsEmpty();
		}

		public object GetSelectedData()
		{
			object Result = null;
			if (this.FList.SelectedItem() != null)
			{
				Result = this.FList.SelectedItem().Data;
			}
			return Result;
		}

		private void SetReadOnly([In] bool Value)
		{
			this.FReadOnly = Value;
			this.FBtnAdd.Enabled = !this.FReadOnly;
			this.FBtnDelete.Enabled = !this.FReadOnly;
			this.FBtnEdit.Enabled = !this.FReadOnly;
			this.FBtnMoveUp.Enabled = !this.FReadOnly;
			this.FBtnMoveDown.Enabled = !this.FReadOnly;
			if (this.FReadOnly)
			{
				this.FList.BackColor = SystemColors.Control;
			}
			else
			{
				this.FList.BackColor = SystemColors.Window;
			}
		}

		public TSheetList(Control AOwner)
		{
			AOwner.SuspendLayout();
			this.Dock = DockStyle.Fill;
			AOwner.Controls.Add(this);
			AOwner.ResumeLayout(false);
			base.SuspendLayout();
			this.FBtnMoveDown = new ToolBarButton();
			this.FBtnMoveDown.ImageIndex = 30;
			this.FBtnMoveDown.ToolTipText = GKL.LSList[300];
			this.FBtnMoveUp = new ToolBarButton();
			this.FBtnMoveUp.ImageIndex = 29;
			this.FBtnMoveUp.ToolTipText = GKL.LSList[299];
			this.FBtnLinkJump = new ToolBarButton();
			this.FBtnLinkJump.ImageIndex = 28;
			this.FBtnLinkJump.ToolTipText = GKL.LSList[298];
			this.FBtnDelete = new ToolBarButton();
			this.FBtnDelete.ImageIndex = 5;
			this.FBtnDelete.ToolTipText = GKL.LSList[21];
			this.FBtnEdit = new ToolBarButton();
			this.FBtnEdit.ImageIndex = 4;
			this.FBtnEdit.ToolTipText = GKL.LSList[20];
			this.FBtnAdd = new ToolBarButton();
			this.FBtnAdd.ImageIndex = 3;
			this.FBtnAdd.ToolTipText = GKL.LSList[19];
			this.FToolBar = new ToolBar();
			this.FToolBar.Appearance = ToolBarAppearance.Flat;
			ToolBar.ToolBarButtonCollection arg_1B2_0 = this.FToolBar.Buttons;
			ToolBarButton[] array = null;
			ToolBarButton[] array2 = array;
			ToolBarButton[] array3;
			ToolBarButton[] expr_160 = array3 = new ToolBarButton[6];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 6)
				{
					num = 6;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_160;
			array[0] = this.FBtnAdd;
			array[1] = this.FBtnEdit;
			array[2] = this.FBtnDelete;
			array[3] = this.FBtnLinkJump;
			array[4] = this.FBtnMoveUp;
			array[5] = this.FBtnMoveDown;
			arg_1B2_0.AddRange(array);
			this.FToolBar.ImageList = GKUI.TfmGEDKeeper.Instance.ImageList_Buttons;
			this.FToolBar.ShowToolTips = true;
			this.FToolBar.ButtonClick += new ToolBarButtonClickEventHandler(this.ButtonClick);
			this.FList = new TGKListView(null);
			this.FList.Location = new Point(0, 0);
			this.FList.Size = new Size(500, 290);
			this.FList.HideSelection = false;
			this.FList.LabelEdit = false;
			this.FList.FullRowSelect = true;
			this.FList.View = View.Details;
			this.FList.DoubleClick += new EventHandler(this.List_DoubleClick);
			this.FList.KeyDown += new KeyEventHandler(this.List_KeyDown);
			this.FToolBar.Dock = DockStyle.Right;
			this.FList.Dock = DockStyle.Fill;
			base.Controls.Add(this.FList);
			base.Controls.Add(this.FToolBar);
			base.Controls.SetChildIndex(this.FList, 0);
			base.Controls.SetChildIndex(this.FToolBar, 1);
			base.ResumeLayout(false);
			this.SetButtons(TEnumSet.Create(new Enum[]
			{
				TSheetList.TListButton.lbAdd, 
				TSheetList.TListButton.lbEdit, 
				TSheetList.TListButton.lbDelete
			}));
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				TObjectHelper.Free(this.FList);
				TObjectHelper.Free(this.FBtnLinkJump);
				TObjectHelper.Free(this.FBtnMoveUp);
				TObjectHelper.Free(this.FBtnMoveDown);
				TObjectHelper.Free(this.FBtnDelete);
				TObjectHelper.Free(this.FBtnEdit);
				TObjectHelper.Free(this.FBtnAdd);
				TObjectHelper.Free(this.FToolBar);
			}
			base.Dispose(Disposing);
		}

		private void ItemAdd()
		{
			if (!this.FReadOnly && this.FOnModify != null)
			{
				this.FOnModify(this, null, TGenEngine.TRecAction.raAdd);
			}
		}

		private void ItemEdit()
		{
			if (!this.FReadOnly && this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raEdit);
			}
		}

		private void ItemDelete()
		{
			if (!this.FReadOnly && this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raDelete);
			}
		}

		private void ItemJump()
		{
			if (this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raJump);
			}
		}

		private void ItemMoveUp()
		{
			if (!this.FReadOnly && this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raMoveUp);
			}
		}

		private void ItemMoveDown()
		{
			if (!this.FReadOnly && this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raMoveDown);
			}
		}

		public void Columns_BeginUpdate()
		{
		}

		public void Columns_Clear()
		{
			if (this.FList != null)
			{
				this.FList.Columns.Clear();
			}
		}

		public void Columns_EndUpdate()
		{
		}

		public void AddColumn(string aCaption, int aWidth, bool aAutoSize)
		{
			this.FList.AddListColumn(aCaption, aWidth, aAutoSize);
		}
	}
}
