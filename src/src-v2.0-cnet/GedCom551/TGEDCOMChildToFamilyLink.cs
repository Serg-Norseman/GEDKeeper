using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMChildToFamilyLink : TGEDCOMPointerWithNotes
	{

		public enum TGEDCOMPedigreeLinkageType : byte
		{
			plNone,
			plAdopted,
			plBirth,
			plFoster,
			plSealing
		}
		public enum TGEDCOMChildLinkageStatus : byte
		{
			clNone,
			clChallenged,
			clDisproven,
			clProven
		}
		[Browsable(false)]
		public TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus ChildLinkageStatus
		{
			get
			{
				return this.GetChildLinkageStatus();
			}
			set
			{
				this.SetChildLinkageStatus(value);
			}
		}
		[Browsable(false)]
		public TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType PedigreeLinkageType
		{
			get
			{
				return this.GetPedigreeLinkageType();
			}
			set
			{
				this.SetPedigreeLinkageType(value);
			}
		}
		[Browsable(false)]
		public TGEDCOMFamilyRecord Family
		{
			get
			{
				return this.GetFamily();
			}
			set
			{
				this.SetFamily(value);
			}
		}
		internal TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType GetPedigreeLinkageType()
		{
			string S = base.GetTagStringValue("PEDI").Trim().ToLower();
			TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType Result;
			if (BDSSystem.WStrCmp(S, "adopted") == 0)
			{
				Result = TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plAdopted;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "birth") == 0)
				{
					Result = TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plBirth;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "foster") == 0)
					{
						Result = TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plFoster;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "sealing") == 0)
						{
							Result = TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plSealing;
						}
						else
						{
							Result = TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plNone;
						}
					}
				}
			}
			return Result;
		}
		internal void SetPedigreeLinkageType([In] TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType Value)
		{
			string S;
			if (Value != TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plAdopted)
			{
				if (Value != TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plBirth)
				{
					if (Value != TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plFoster)
					{
						if (Value != TGEDCOMChildToFamilyLink.TGEDCOMPedigreeLinkageType.plSealing)
						{
							S = "";
						}
						else
						{
							S = "sealing";
						}
					}
					else
					{
						S = "foster";
					}
				}
				else
				{
					S = "birth";
				}
			}
			else
			{
				S = "adopted";
			}
			base.SetTagStringValue("PEDI", S);
		}
		internal TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus GetChildLinkageStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToLower();
			TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus Result;
			if (BDSSystem.WStrCmp(S, "challenged") == 0)
			{
				Result = TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus.clChallenged;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "disproven") == 0)
				{
					Result = TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus.clDisproven;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "proven") == 0)
					{
						Result = TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus.clProven;
					}
					else
					{
						Result = TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus.clNone;
					}
				}
			}
			return Result;
		}
		internal void SetChildLinkageStatus([In] TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus Value)
		{
			string S;
			if (Value != TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus.clChallenged)
			{
				if (Value != TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus.clDisproven)
				{
					if (Value != TGEDCOMChildToFamilyLink.TGEDCOMChildLinkageStatus.clProven)
					{
						S = "";
					}
					else
					{
						S = "proven";
					}
				}
				else
				{
					S = "disproven";
				}
			}
			else
			{
				S = "challenged";
			}
			base.SetTagStringValue("STAT", S);
		}
		internal TGEDCOMFamilyRecord GetFamily()
		{
			return base.GetValue() as TGEDCOMFamilyRecord;
		}
		internal void SetFamily([In] TGEDCOMFamilyRecord Value)
		{
			base.SetValue(Value);
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "FAMC";
		}

		public TGEDCOMChildToFamilyLink(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
