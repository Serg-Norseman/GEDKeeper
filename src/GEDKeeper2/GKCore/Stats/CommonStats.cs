using System;

namespace GKCore.Stats
{
	/// <summary>
	/// 
	/// </summary>
	public sealed class CommonStats
	{
		public int persons;
		public int persons_m;
		public int persons_f;
		public int lives;
		public int lives_m;
		public int lives_f;

		public readonly CompositeItem age;
		public readonly CompositeItem life;
		public readonly CompositeItem childs;
		public readonly CompositeItem fba;
		public readonly CompositeItem marr;
		public readonly CompositeItem mage;
		public readonly CompositeItem cIndex;
		
		public CommonStats()
		{
			this.persons = 0;
			this.persons_m = 0;
			this.persons_f = 0;
			this.lives = 0;
			this.lives_m = 0;
			this.lives_f = 0;

			this.age = new CompositeItem();
			this.life = new CompositeItem();
			this.childs = new CompositeItem();
			this.fba = new CompositeItem();
			this.marr = new CompositeItem();
			this.mage = new CompositeItem();
			this.cIndex = new CompositeItem();
		}
	}
}
