using System;

/// <summary>
/// Localization: clean
/// </summary>

namespace ExtUtils
{
	public struct EnumSet
	{
		private uint fValue;

		public static EnumSet Create()
		{
			EnumSet result = new EnumSet();
			result.fValue = 0;
			return result;
		}

		public static EnumSet Create(params Enum[] e)
		{
			EnumSet result = EnumSet.Create();
			result.Include(e);
			return result;
		}

		public void Include(params Enum[] e)
		{
			for (int i = 0; i <= e.Length - 1; i++) {
				this.Include(e[i]);
			}
		}

		public void Include(Enum e)
		{
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				this.fValue |= (uint)(1 << pos);
			}
		}

		public void Exclude(Enum e)
		{
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				this.fValue &= (uint)(1 << (int)pos ^ -1);
			}
		}

		public bool InSet(Enum e)
		{
			unchecked
			{
				byte pos = ((IConvertible)e).ToByte(null);
				uint bt = (uint)(1 << pos);
				return (bt & this.fValue) > 0u;
			}
		}

		public bool IsEmpty()
		{
			return this.fValue == 0;
		}

		public override string ToString()
		{
			uint B = (byte)this.fValue;
			uint bt = 1;
			string s = "";
			int i = 1;

			do
			{
				if ((B & bt) > 0) {
					s = "1" + s;
				} else {
					s = "0" + s;
				}

				bt = bt << 1;
				i++;
			}
			while (i != 9);

			return s;
		}

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            EnumSet es = (EnumSet)obj;
            return (this.fValue == es.fValue);
        }
	}

}
