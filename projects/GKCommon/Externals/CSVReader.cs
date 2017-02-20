/* CSVReader - a simple open source C# class library to read CSV data
 * by Andrew Stellman - http://www.stellman-greene.com/CSVReader
 * 
 * CSVReader.cs - Class to read CSV data from a string, file or stream
 * 
 * download the latest version: http://svn.stellman-greene.com/CSVReader
 * 
 * (c) 2008, Stellman & Greene Consulting
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Stellman & Greene Consulting nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY STELLMAN & GREENE CONSULTING ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL STELLMAN & GREENE CONSULTING BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */

using System;
using System.Collections.Generic;
using System.Data;
using System.Globalization;
using System.IO;
using System.Text;
using System.Threading;

namespace Externals
{
    /// <summary>
    /// Read CSV-formatted data from a file or TextReader
    /// </summary>
    public sealed class CSVReader : IDisposable
    {
        private const string NEWLINE = "\r\n";

        /// <summary>
        /// This reader will read all of the CSV data
        /// </summary>
        private readonly BinaryReader reader;

        #region Instance control

        /// <summary>
        /// Read CSV-formatted data from a file
        /// </summary>
        /// <param name="csvFileInfo">Name of the CSV file</param>
        public CSVReader(FileInfo csvFileInfo)
        {
            if (csvFileInfo == null)
                throw new ArgumentNullException("csvFileInfo", @"Null FileInfo passed to CSVReader");

            reader = new BinaryReader(File.OpenRead(csvFileInfo.FullName));
        }

        /// <summary>
        /// Read CSV-formatted data from a string
        /// </summary>
        /// <param name="csvData">String containing CSV data</param>
        public CSVReader(string csvData)
        {
            if (csvData == null)
                throw new ArgumentNullException("csvData", @"Null string passed to CSVReader");

            reader = new BinaryReader(new MemoryStream(Encoding.UTF8.GetBytes(csvData)));
        }

        /// <summary>
        /// Read CSV-formatted data from a TextReader
        /// </summary>
        /// <param name="reader">TextReader that's reading CSV-formatted data</param>
        public CSVReader(TextReader reader)
        {
            if (reader == null)
                throw new ArgumentNullException("reader", @"Null TextReader passed to CSVReader");

            this.reader = new BinaryReader(new MemoryStream(Encoding.UTF8.GetBytes(reader.ReadToEnd())));
        }

        public void Dispose()
        {
            try
            {
                // Can't call BinaryReader.Dispose due to its protection level
                if (reader != null) reader.Close();
            }
            catch { }
        }

        /// <summary>
        /// Read a CSV file into a table
        /// </summary>
        /// <param name="filename">Filename of CSV file</param>
        /// <param name="headerRow">True if the first row contains column names</param>
        /// <returns>System.Data.DataTable object that contains the CSV data</returns>
        public static DataTable ReadCSVFile(string filename, bool headerRow)
        {
            using (CSVReader reader = new CSVReader(new FileInfo(filename)))
                return reader.CreateDataTable(headerRow);
        }

        #endregion

        #region Data reading

        private string currentLine = "";

        /// <summary>
        /// Read the next row from the CSV data
        /// </summary>
        /// <returns>A list of objects read from the row, or null if there is no next row</returns>
        public List<object> ReadRow()
        {
            // ReadLine() will return null if there's no next line
            if (reader.BaseStream.Position >= reader.BaseStream.Length)
                return null;

            StringBuilder builder = new StringBuilder();

            // Read the next line
            while ((reader.BaseStream.Position < reader.BaseStream.Length) && (!builder.ToString().EndsWith(NEWLINE)))
            {
                char c = reader.ReadChar();
                builder.Append(c);
            }

            currentLine = builder.ToString();
            if (currentLine.EndsWith(NEWLINE))
                currentLine = currentLine.Remove(currentLine.IndexOf(NEWLINE), NEWLINE.Length);

            // Build the list of objects in the line
            List<object> objects = new List<object>();
            while (currentLine != "")
                objects.Add(ReadNextObject());
            return objects;
        }

        /// <summary>
        /// Read the next object from the currentLine string
        /// </summary>
        /// <returns>The next object in the currentLine string</returns>
        private object ReadNextObject()
        {
            if (currentLine == null)
                return null;

            // Check to see if the next value is quoted
            bool quoted = currentLine.StartsWith("\"");

            // Find the end of the next value
            int i = 0;
            int len = currentLine.Length;
            bool foundEnd = false;
            while (!foundEnd && i <= len)
            {
                // Check if we've hit the end of the string
                if ((!quoted && i == len) // non-quoted strings end with a comma or end of line
                    || (!quoted && currentLine.Substring(i, 1) == ",")
                    // quoted strings end with a quote followed by a comma or end of line
                    || (quoted && i == len - 1 && currentLine.EndsWith("\""))
                    || (quoted && currentLine.Substring(i, 2) == "\","))
                    foundEnd = true;
                else
                    i++;
            }
            if (quoted)
            {
                if (i > len || !currentLine.Substring(i, 1).StartsWith("\""))
                    throw new FormatException("Invalid CSV format: " + currentLine.Substring(0, i));
                i++;
            }

            string nextObjectString = currentLine.Substring(0, i).Replace("\"\"", "\"");

            currentLine = (i < len) ? currentLine.Substring(i + 1) : "";

            if (quoted)
            {
                if (nextObjectString.StartsWith("\""))
                    nextObjectString = nextObjectString.Substring(1);
                if (nextObjectString.EndsWith("\""))
                    nextObjectString = nextObjectString.Substring(0, nextObjectString.Length - 1);
                return nextObjectString;
            }
            else
            {
                object convertedValue = ConvertString(nextObjectString);
                return convertedValue;
            }
        }

        /// <summary>
        /// Read the row data read using repeated ReadRow() calls and build a DataColumnCollection with types and column names
        /// </summary>
        /// <param name="headerRow">True if the first row contains headers</param>
        /// <returns>System.Data.DataTable object populated with the row data</returns>
        public DataTable CreateDataTable(bool headerRow)
        {
            // Read the CSV data into rows
            List<List<object>> rows = new List<List<object>>();

            List<object> readRow;
            while ((readRow = ReadRow()) != null)
                rows.Add(readRow);

            // The types and names (if headerRow is true) will be stored in these lists
            List<Type> columnTypes = new List<Type>();
            List<string> columnNames = new List<string>();

            // Read the column names from the header row (if there is one)
            if (headerRow)
                foreach (object name in rows[0])
                    columnNames.Add(name.ToString());

            // Read the column types from each row in the list of rows
            bool headerRead = false;
            foreach (List<object> row in rows)
                if (headerRead || !headerRow)
                    for (int i = 0; i < row.Count; i++)
                        // If we're adding a new column to the columnTypes list, use its type.
                        // Otherwise, find the common type between the one that's there and the new row.
                        if (columnTypes.Count < i + 1)
                            columnTypes.Add(row[i].GetType());
                        else
                            columnTypes[i] = FindCommonType(columnTypes[i], row[i].GetType());
                        else
                            headerRead = true;

            // Create the table and add the columns
            DataTable table = new DataTable();
            for (int i = 0; i < columnTypes.Count; i++)
            {
                table.Columns.Add();
                table.Columns[i].DataType = columnTypes[i];
                if (i < columnNames.Count)
                    table.Columns[i].ColumnName = columnNames[i];
            }

            // Add the data from the rows
            headerRead = false;
            foreach (List<object> row in rows)
                if (headerRead || !headerRow)
            {
                DataRow dataRow = table.NewRow();
                for (int i = 0; i < row.Count; i++)
                    dataRow[i] = row[i];
                table.Rows.Add(dataRow);
            }
            else
                headerRead = true;

            return table;
        }

        #endregion

        #region StringConverter

        private static object ConvertString(string value)
        {
            // First check the whole number types, because floating point types will always parse whole numbers
            // Start with the smallest types
            byte byteResult;
            if (byte.TryParse(value, out byteResult))
            {
                return byteResult;
            }

            short shortResult;
            if (short.TryParse(value, out shortResult))
            {
                return shortResult;
            }

            int intResult;
            if (int.TryParse(value, out intResult))
            {
                return intResult;
            }

            long longResult;
            if (long.TryParse(value, out longResult))
            {
                return longResult;
            }

            ulong ulongResult;
            if (ulong.TryParse(value, out ulongResult))
            {
                return ulongResult;
            }

            // No need to check the rest of the unsigned types, which will fit into the signed whole number types

            // In those files there is only the comma and semicolon as delimiters of values.
            // Therefore, the decimal separator can be only the dot.
            NumberFormatInfo formatInfo = (NumberFormatInfo)Thread.CurrentThread.CurrentCulture.NumberFormat.Clone();
            formatInfo.NumberDecimalSeparator = ".";
            formatInfo.NumberGroupSeparator = "";

            // Next check the floating point types
            float floatResult;
            if (float.TryParse(value, NumberStyles.Float, formatInfo, out floatResult))
            {
                return floatResult;
            }

            // It's not clear that there's anything that double.TryParse() and decimal.TryParse() will parse
            // but which float.TryParse() won't
            double doubleResult;
            if (double.TryParse(value, NumberStyles.Float, formatInfo, out doubleResult))
            {
                return doubleResult;
            }

            decimal decimalResult;
            if (decimal.TryParse(value, NumberStyles.Float, formatInfo, out decimalResult))
            {
                return decimalResult;
            }

            // It's not a number, so it's either a bool, char or string
            bool boolResult;
            if (bool.TryParse(value, out boolResult))
            {
                return boolResult;
            }

            char charResult;
            if (char.TryParse(value, out charResult))
            {
                return charResult;
            }

            return value;
        }

        /// <summary>
        /// Compare two types and find a type that can fit both of them
        /// </summary>
        /// <param name="typeA">First type to compare</param>
        /// <param name="typeB">Second type to compare</param>
        /// <returns>The type that can fit both types, or string if they're incompatible</returns>
        private static Type FindCommonType(Type typeA, Type typeB)
        {
            // Build the singleton type map (which will rebuild it in a typesafe manner
            // if it's not already built).
            BuildTypeMap();

            if (!typeMap.ContainsKey(typeA))
                return typeof(string);

            if (!typeMap[typeA].ContainsKey(typeB))
                return typeof(string);

            return typeMap[typeA][typeB];
        }


        // Dictionary to map two types to a common type that can hold both of them
        private static Dictionary<Type, Dictionary<Type, Type>> typeMap;

        // Locker object to build the singleton typeMap in a typesafe manner
        private static readonly object locker = new object();

        /// <summary>
        /// Build the singleton type map in a typesafe manner.
        /// This map is a dictionary that maps a pair of types to a common type.
        /// So typeMap[typeof(float)][typeof(uint)] will return float, while
        /// typemap[typeof(char)][typeof(bool)] will return string.
        /// </summary>
        private static void BuildTypeMap()
        {
            lock (locker)
            {
                if (typeMap == null)
                {
                    typeMap = new Dictionary<Type, Dictionary<Type, Type>>()
                    {
                        // Comparing byte
                        {typeof(byte), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(byte) },
                                { typeof(short), typeof(short) },
                                { typeof(int), typeof(int) },
                                { typeof(long), typeof(long) },
                                { typeof(ulong), typeof(ulong) },
                                { typeof(float), typeof(float) },
                                { typeof(double), typeof(double) },
                                { typeof(decimal), typeof(decimal) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing short
                        {typeof(short), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(short) },
                                { typeof(short), typeof(short) },
                                { typeof(int), typeof(int) },
                                { typeof(long), typeof(long) },
                                { typeof(ulong), typeof(ulong) },
                                { typeof(float), typeof(float) },
                                { typeof(double), typeof(double) },
                                { typeof(decimal), typeof(decimal) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing int
                        {typeof(int), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(int) },
                                { typeof(short), typeof(int) },
                                { typeof(int), typeof(int) },
                                { typeof(long), typeof(long) },
                                { typeof(ulong), typeof(ulong) },
                                { typeof(float), typeof(float) },
                                { typeof(double), typeof(double) },
                                { typeof(decimal), typeof(decimal) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing long
                        {typeof(long), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(long) },
                                { typeof(short), typeof(long) },
                                { typeof(int), typeof(long) },
                                { typeof(long), typeof(long) },
                                { typeof(ulong), typeof(ulong) },
                                { typeof(float), typeof(float) },
                                { typeof(double), typeof(double) },
                                { typeof(decimal), typeof(decimal) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing ulong
                        {typeof(ulong), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(ulong) },
                                { typeof(short), typeof(ulong) },
                                { typeof(int), typeof(ulong) },
                                { typeof(long), typeof(ulong) },
                                { typeof(ulong), typeof(ulong) },
                                { typeof(float), typeof(float) },
                                { typeof(double), typeof(double) },
                                { typeof(decimal), typeof(decimal) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing float
                        {typeof(float), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(float) },
                                { typeof(short), typeof(float) },
                                { typeof(int), typeof(float) },
                                { typeof(long), typeof(float) },
                                { typeof(ulong), typeof(float) },
                                { typeof(float), typeof(float) },
                                { typeof(double), typeof(double) },
                                { typeof(decimal), typeof(decimal) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing double
                        {typeof(double), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(double) },
                                { typeof(short), typeof(double) },
                                { typeof(int), typeof(double) },
                                { typeof(long), typeof(double) },
                                { typeof(ulong), typeof(double) },
                                { typeof(float), typeof(double) },
                                { typeof(double), typeof(double) },
                                { typeof(decimal), typeof(decimal) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing decimal
                        {typeof(decimal), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(decimal) },
                                { typeof(short), typeof(decimal) },
                                { typeof(int), typeof(decimal) },
                                { typeof(long), typeof(decimal) },
                                { typeof(ulong), typeof(decimal) },
                                { typeof(float), typeof(decimal) },
                                { typeof(double), typeof(decimal) },
                                { typeof(decimal), typeof(decimal) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing bool
                        {typeof(bool), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(string) },
                                { typeof(short), typeof(string) },
                                { typeof(int), typeof(string) },
                                { typeof(long), typeof(string) },
                                { typeof(ulong), typeof(string) },
                                { typeof(float), typeof(string) },
                                { typeof(double), typeof(string) },
                                { typeof(decimal), typeof(string) },
                                { typeof(bool), typeof(bool) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing char
                        {typeof(char), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(string) },
                                { typeof(short), typeof(string) },
                                { typeof(int), typeof(string) },
                                { typeof(long), typeof(string) },
                                { typeof(ulong), typeof(string) },
                                { typeof(float), typeof(string) },
                                { typeof(double), typeof(string) },
                                { typeof(decimal), typeof(string) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(char) },
                                { typeof(string), typeof(string) }
                            }},

                        // Comparing string
                        {typeof(string), new Dictionary<Type, Type>() {
                                { typeof(byte), typeof(string) },
                                { typeof(short), typeof(string) },
                                { typeof(int), typeof(string) },
                                { typeof(long), typeof(string) },
                                { typeof(ulong), typeof(string) },
                                { typeof(float), typeof(string) },
                                { typeof(double), typeof(string) },
                                { typeof(decimal), typeof(string) },
                                { typeof(bool), typeof(string) },
                                { typeof(char), typeof(string) },
                                { typeof(string), typeof(string) }
                            }}
                    };
                }
            }
        }

        #endregion
    }
}
