using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Data.Sqlite;
using Microsoft.EntityFrameworkCore;

namespace DIPolWeb.FetchData.DataBase
{
    internal sealed class SqLiteSource : DbContext
    {
        public DbSet<DbEntry> Obslog { get; set; }
        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            base.OnConfiguring(optionsBuilder);
            optionsBuilder.UseSqlite(
                new SqliteConnection()
                {
                    ConnectionString = new SqliteConnectionStringBuilder()
                    {
                        DataSource = Path.Combine("sample-data", "obslog.sqlite"),
                        Mode = SqliteOpenMode.ReadOnly

                    }.ToString()
                }
            );
        }
        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<DbEntry>().ToTable("obslog");
            modelBuilder.Entity<DbEntry>().Property(entry => entry.Date).HasConversion(to => to.ToString("yyyy-MM-dd"), from => DateOnly.Parse(from));
        }

        public async Task RunEFCore()
        {
            if (!await Database.EnsureCreatedAsync())
            {
                throw new InvalidOperationException("Failed to load database");
            }
        }

        public IAsyncEnumerable<DbEntry> GetData() => Obslog.AsAsyncEnumerable();
    }
}
