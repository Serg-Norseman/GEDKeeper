using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMTime : TGEDCOMTag
	{
		private ushort FHour;
		private ushort FMinutes;
		private ushort FSeconds;
		private ushort FFraction;

		public ushort Fraction
		{
			get { return this.FFraction; }
			set { this.FFraction = value; }
		}

		public ushort Hour
		{
			get { return this.FHour; }
			set { this.FHour = value; }
		}

		public ushort Minutes
		{
			get { return this.FMinutes; }
			set { this.FMinutes = value; }
		}

		public ushort Seconds
		{
			get { return this.FSeconds; }
			set { this.FSeconds = value; }
		}

		public TimeSpan Time
		{
			get { return this.GetValue(); }
			set { this.SetValue(value); }
		}

		public TimeSpan Value
		{
			get { return this.GetValue(); }
			set { this.SetValue(value); }
		}

		private TimeSpan GetValue()
		{
			return new TimeSpan(0, (int)this.FHour, (int)this.FMinutes, (int)this.FSeconds, (int)(100u * (uint)this.FFraction));
		}

		private void SetValue(TimeSpan AValue)
		{
			this.FHour = (ushort)AValue.Hours;
			this.FMinutes = (ushort)AValue.Minutes;
			this.FSeconds = (ushort)AValue.Seconds;
			ushort MSec = (ushort)AValue.Milliseconds;
			this.FFraction = (ushort)Math.Truncate(MSec / 100.0);
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "TIME";
		}

		protected override string GetStringValue()
		{
			string Result;
			if (this.FHour == 0 && this.FMinutes == 0 && this.FSeconds == 0)
			{
				Result = "";
			}
			else
			{
				Result = string.Format("{0:00}:{1:00}:{2:00}", new object[] { this.FHour, this.FMinutes, this.FSeconds });

				if (this.FFraction > 0)
				{
					Result = Result + "." + this.FFraction.ToString();
				}
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			this.FHour = 0;
			this.FMinutes = 0;
			this.FSeconds = 0;
			this.FFraction = 0;
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.FHour == 0 && this.FMinutes == 0 && this.FSeconds == 0;
		}

		public override string ParseString([In] string AString)
		{
			this.FHour = 0;
			this.FMinutes = 0;
			this.FSeconds = 0;
			this.FFraction = 0;

			string result = AString;
			if (!string.IsNullOrEmpty(result))
			{
				result = base.ExtractDelimiter(result, 0);

				int H;
				result = TGEDCOMObject.ExtractNumber(result, out H, false, 0);
				this.FHour = (ushort)H;
				if (result != "" && result[0] == ':')
				{
					result = result.Remove(0, 1);
				}

				int M;
				result = TGEDCOMObject.ExtractNumber(result, out M, false, 0);
				this.FMinutes = (ushort)M;
				if (result != "" && result[0] == ':')
				{
					result = result.Remove(0, 1);

					int S;
					result = TGEDCOMObject.ExtractNumber(result, out S, false, 0);
					this.FSeconds = (ushort)S;
					if (result != "" && result[0] == '.')
					{
						result = result.Remove(0, 1);

						int F;
						result = TGEDCOMObject.ExtractNumber(result, out F, false, 0);
						this.FFraction = (ushort)F;
					}
				}
			}
			return result;
		}

		public TGEDCOMTime(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMTime(AOwner, AParent, AName, AValue);
		}
	}
}
