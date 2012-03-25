using System;
using System.Diagnostics;
using System.IO;
using System.Management;
using System.Security.Permissions;

namespace GKSandbox
{
	public class SystemInfo
	{
		public string InsertThousands(string s)
		{
			for (int i = s.Length - 3; i > 0; i -= 3)
			{
				s = s.Insert(i, " ");
			}
			return s;
		}

		public string GetProcessorInfo()
		{
			string res = "";

			ManagementObjectSearcher managementObjectSearcher = new ManagementObjectSearcher("SELECT * FROM Win32_Processor");
			using (ManagementObjectCollection.ManagementObjectEnumerator enumerator = managementObjectSearcher.Get().GetEnumerator())
			{
				while (enumerator.MoveNext())
				{
					ManagementObject manObject = (ManagementObject)enumerator.Current;

					res = "\n=== Processor information ===";
					res += "\nManufacturer:\t\t" + manObject["Manufacturer"].ToString();
					res += "\nClockSpeed:\t\t" + manObject["MaxClockSpeed"].ToString();
					res += "\nCurrentClockSpeed:\t" + manObject["CurrentClockSpeed"].ToString();
					res += "\nFamily:\t\t\t" + manObject["Family"].ToString();
					res += "\nVersion:\t\t" + manObject["Version"].ToString();
					res += "\nProcessorId:\t\t" + manObject["ProcessorId"].ToString();
					res += "\n";
				}
			}

			return res;
		}

		public string GetRAMInfo()
		{
			long total = 0;
			ManagementScope scope = new ManagementScope();
			ObjectQuery query = new ObjectQuery("SELECT Capacity FROM Win32_PhysicalMemory");
			ManagementObjectSearcher managementObjectSearcher = new ManagementObjectSearcher(scope, query);
			ManagementObjectCollection managementObjectCollection = managementObjectSearcher.Get();
			using (ManagementObjectCollection.ManagementObjectEnumerator enumerator = managementObjectCollection.GetEnumerator())
			{
				while (enumerator.MoveNext())
				{
					ManagementObject managementObject = (ManagementObject)enumerator.Current;
					total += uint.Parse(managementObject["Capacity"].ToString());
				}
			}

			new PermissionSetAttribute(SecurityAction.Assert);
			PerformanceCounter ramCounter = new PerformanceCounter();
			ramCounter.CategoryName = "Memory";
			ramCounter.CounterName = "Available Bytes";
			ramCounter.ReadOnly = true;
			long num = Convert.ToInt64(ramCounter.NextValue());

			string res = "=== Memory ===";
			res += "\nInstalled:\t\t" + InsertThousands(total.ToString()) + " bytes";
			res += "\nAvailable:\t\t" + InsertThousands(num.ToString()) + " bytes";
			return res;
		}

		public string GetComputerInfo()
		{
			string text = "=== Computer ===";
			ObjectQuery query = new ObjectQuery("SELECT * FROM Win32_OperatingSystem");
			ManagementScope scope = new ManagementScope();
			ManagementObjectSearcher managementObjectSearcher = new ManagementObjectSearcher(scope, query);
			ManagementObjectCollection managementObjectCollection = managementObjectSearcher.Get();
			using (ManagementObjectCollection.ManagementObjectEnumerator enumerator = managementObjectCollection.GetEnumerator())
			{
				while (enumerator.MoveNext())
				{
					ManagementObject managementObject = (ManagementObject)enumerator.Current;
					text = text + "\nComputer Name:\t" + managementObject["csname"];
					text = text + "\nWindows Directory:\t" + managementObject["WindowsDirectory"];
					text = text + "\nOperating System:\t" + managementObject["Caption"];
					text = text + "\nVersion:\t\t" + managementObject["Version"];
					text = text + "\nManufacturer:\t\t" + managementObject["Manufacturer"];
				}
			}
			return text;
		}

	}
}
