using System;

namespace GedCom551
{
	public sealed class TGEDCOMTime : TGEDCOMTag
	{
		private ushort fHour;
		private ushort fMinutes;
		private ushort fSeconds;
		private ushort fFraction;

		public ushort Fraction
		{
			get { return this.fFraction; }
			set { this.fFraction = value; }
		}

		public ushort Hour
		{
			get { return this.fHour; }
			set { this.fHour = value; }
		}

		public ushort Minutes
		{
			get { return this.fMinutes; }
			set { this.fMinutes = value; }
		}

		public ushort Seconds
		{
			get { return this.fSeconds; }
			set { this.fSeconds = value; }
		}

		public TimeSpan Value
		{
			get {
				return new TimeSpan(0, (int)this.fHour, (int)this.fMinutes, (int)this.fSeconds, (int)(100u * (uint)this.fFraction));
			}
			set {
				this.fHour = (ushort)value.Hours;
				this.fMinutes = (ushort)value.Minutes;
				this.fSeconds = (ushort)value.Seconds;
				ushort mSec = (ushort)value.Milliseconds;
				this.fFraction = (ushort)Math.Truncate(mSec / 100.0);
			}
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "TIME";
		}

		protected override string GetStringValue()
		{
			string result;
			if (this.fHour == 0 && this.fMinutes == 0 && this.fSeconds == 0)
			{
				result = "";
			}
			else
			{
				result = string.Format("{0:00}:{1:00}:{2:00}", new object[] { this.fHour, this.fMinutes, this.fSeconds });

				if (this.fFraction > 0)
				{
					result = result + "." + this.fFraction.ToString();
				}
			}
			return result;
		}

		public override void Clear()
		{
			base.Clear();
			this.fHour = 0;
			this.fMinutes = 0;
			this.fSeconds = 0;
			this.fFraction = 0;
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fHour == 0 && this.fMinutes == 0 && this.fSeconds == 0;
		}

		public override string ParseString(string strValue)
		{
			this.fHour = 0;
			this.fMinutes = 0;
			this.fSeconds = 0;
			this.fFraction = 0;

			string result = strValue;
			if (!string.IsNullOrEmpty(result))
			{
				result = GEDCOMUtils.ExtractDelimiter(result, 0);

				int tmp;
				result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
				this.fHour = (ushort)tmp;
				if (result != "" && result[0] == ':')
				{
					result = result.Remove(0, 1);
				}

				result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
				this.fMinutes = (ushort)tmp;
				if (result != "" && result[0] == ':')
				{
					result = result.Remove(0, 1);

					result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
					this.fSeconds = (ushort)tmp;
					if (result != "" && result[0] == '.')
					{
						result = result.Remove(0, 1);

						result = GEDCOMUtils.ExtractNumber(result, out tmp, false, 0);
						this.fFraction = (ushort)tmp;
					}
				}
			}
			return result;
		}

		public TGEDCOMTime(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMTime(owner, parent, tagName, tagValue);
		}
	}
}
