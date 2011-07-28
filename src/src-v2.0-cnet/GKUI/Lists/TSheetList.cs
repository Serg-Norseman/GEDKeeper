using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI.Lists
{
	public class TSheetList : ContainerControl, IDisposable
	{
		public delegate void TModifyEvent(object Sender, object ItemData, TGenEngine.TRecAction Action);

		internal enum TListButton : byte
		{
			lbAdd,
			lbEdit,
			lbDelete,
			lbJump,
			lbMoveUp,
			lbMoveDown
		}

		internal ToolBarButton FBtnAdd;
		internal ToolBarButton FBtnDelete;
		internal ToolBarButton FBtnEdit;
		internal ToolBarButton FBtnLinkJump;
		internal ToolBarButton FBtnMoveUp;
		internal ToolBarButton FBtnMoveDown;
		internal ToolBar FToolBar;
		internal TEnumSet FButtons;
		internal TGKListView FList;
		internal TSheetList.TModifyEvent FOnModify;
		internal bool FReadOnly;

		[Browsable(false)]
		public event TSheetList.TModifyEvent OnModify
		{
			[MethodImpl(32)]
			add
			{
				this.set_OnModify(value);
			}
			[MethodImpl(32)]
			remove
			{
				if (this.get_OnModify() == value)
				{
					this.set_OnModify(null);
				}
			}
		}
		[Browsable(false)]
		public TEnumSet Buttons
		{
			get
			{
				return this.FButtons;
			}
			set
			{
				this.SetButtons(value);
			}
		}
		[Browsable(false)]
		public TGKListView List
		{
			get
			{
				return this.FList;
			}
		}
		[Browsable(false)]
		public ToolBar ToolBar
		{
			get
			{
				return this.FToolBar;
			}
		}
		[Browsable(false)]
		public bool ReadOnly
		{
			get
			{
				return this.FReadOnly;
			}
			set
			{
				this.SetReadOnly(value);
			}
		}
		internal void ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (object.Equals(e.Button, this.FBtnAdd))
			{
				this.ItemAdd();
			}
			else
			{
				if (object.Equals(e.Button, this.FBtnEdit))
				{
					this.ItemEdit();
				}
				else
				{
					if (object.Equals(e.Button, this.FBtnDelete))
					{
						this.ItemDelete();
					}
					else
					{
						if (object.Equals(e.Button, this.FBtnLinkJump))
						{
							this.ItemJump();
						}
						else
						{
							if (object.Equals(e.Button, this.FBtnMoveUp))
							{
								this.ItemMoveUp();
							}
							else
							{
								if (object.Equals(e.Button, this.FBtnMoveDown))
								{
									this.ItemMoveDown();
								}
							}
						}
					}
				}
			}
		}
		internal void List_DoubleClick(object sender, EventArgs e)
		{
			this.ItemEdit();
		}
		internal void List_KeyDown(object sender, KeyEventArgs e)
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
		internal void SheetShow(object sender, EventArgs e)
		{
			this.FList.Focus();
		}
		internal void SetButtons([In] TEnumSet Value)
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
		internal object GetSelectedData()
		{
			object Result = null;
			if (this.FList.SelectedItem() != null)
			{
				Result = this.FList.SelectedItem().Data;
			}
			return Result;
		}
		internal void SetReadOnly([In] bool Value)
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
			this.FToolBar.ImageList = GKL.fmGEDKeeper.ImageList_Buttons;
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

		public void ItemAdd()
		{
			if (!this.FReadOnly && this.FOnModify != null)
			{
				this.FOnModify(this, null, TGenEngine.TRecAction.raAdd);
			}
		}
		public void ItemEdit()
		{
			if (!this.FReadOnly && this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raEdit);
			}
		}
		public void ItemDelete()
		{
			if (!this.FReadOnly && this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raDelete);
			}
		}
		public void ItemJump()
		{
			if (this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raJump);
			}
		}
		public void ItemMoveUp()
		{
			if (!this.FReadOnly && this.FOnModify != null && this.GetSelectedData() != null)
			{
				this.FOnModify(this, this.GetSelectedData(), TGenEngine.TRecAction.raMoveUp);
			}
		}
		public void ItemMoveDown()
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
		[MethodImpl(32)]
		public TSheetList.TModifyEvent get_OnModify()
		{
			return this.FOnModify;
		}

		[MethodImpl(32)]
		public void set_OnModify(TSheetList.TModifyEvent Value)
		{
			this.FOnModify = Value;
		}
	}
}
