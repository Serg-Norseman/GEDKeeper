namespace Externals.Linguistics
{
    /// <summary>
    /// Выбирает правильную форму множественного числа для существительного
    /// </summary>
    public class Pluralizer
    {
        /// <summary>
        /// Выбирает правильную форму множественного числа для существительного
        /// </summary>
        /// <param name="num">Число</param>
        /// <param name="nounForms">3 формы существительного для чисел: 1, 2, 5</param>
        /// <returns></returns>
        public static string Pluralize(int num, string[] nounForms)
        {
            int lastDigit = num % 10;
            int last2Digits = num % 100;
            if (lastDigit == 1)
            {
                if (last2Digits != 11)
                {
                    return nounForms[0];
                }
            }
            else if (2 <= lastDigit && lastDigit <= 4)
            {
                if (last2Digits < 10 || last2Digits > 20)
                {
                    return nounForms[1];
                }
            }
            return nounForms[2];
        }
    }
}
