namespace BSLib
{
	public sealed class WeightedMean
	{
		private double result;
		private double wsum;

		public WeightedMean()
		{
			this.result = 0.0d;
			this.wsum = 0.0d;
		}
		
		public void AddValue(double value, double weight)
		{
			this.result += (value * weight);
			this.wsum += weight;
		}
		
		public double GetResult()
		{
		    return (wsum != 0.0d) ? result / wsum : double.NaN;
		}
	}
}
