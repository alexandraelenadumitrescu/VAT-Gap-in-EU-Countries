import re
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.patheffects as pe
import matplotlib.colors as mcolors
from shapely.geometry import Point

# --- 1. Procesarea Datelor ---
raw_data = "country_codevat_gapshadow_2022AT1.0%6.6%BE12.3%16.0%BG8.6%33.1%CY3.3%23.9%CZ8.0%13.5%DE9.7%8.8%DK8.9%9.7%EE10.3%22.7%EL11.4%20.93%ES7.6%15.8%FI3.0%10.8%FR5.6%14.2%HR7.7%29.7%HU7.4%25.4%IE8.3%10.1%IT15.0%20.3%LT15.1%22.4%LU0.18%8.3%LV5.4%19.9%MT24.2%23.4%NL7.0%8.2%PL16.0%21.9%PT3.6%15.7%RO30.0%29.0%SE5.3%10.8%SI4.9%22.1%SK10.5%13.1%"
pattern = r"([A-Z]{2})([\d\.]+)%([\d\.]+)%"
matches = re.findall(pattern, raw_data)
df = pd.DataFrame(matches, columns=['iso2', 'vat_gap', 'shadow_2022'])
df['vat_gap'] = df['vat_gap'].astype(float)

# --- 2. Încărcarea Hărții ---
url_harta = "https://naturalearth.s3.amazonaws.com/50m_cultural/ne_50m_admin_0_countries.zip"
world = gpd.read_file(url_harta)
# Proiecție oficială europeană (elimină aspectul turtit)
world = world.to_crs(epsg=3035)

# --- 3. Pregătirea Datelor ---
iso_mapping = {
    'AT': 'AUT', 'BE': 'BEL', 'BG': 'BGR', 'CY': 'CYP', 'CZ': 'CZE', 
    'DE': 'DEU', 'DK': 'DNK', 'EE': 'EST', 'EL': 'GRC', 'ES': 'ESP', 
    'FI': 'FIN', 'FR': 'FRA', 'HR': 'HRV', 'HU': 'HUN', 'IE': 'IRL', 
    'IT': 'ITA', 'LT': 'LTU', 'LU': 'LUX', 'LV': 'LVA', 'MT': 'MLT', 
    'NL': 'NLD', 'PL': 'POL', 'PT': 'PRT', 'RO': 'ROU', 'SE': 'SWE', 
    'SI': 'SVN', 'SK': 'SVK'
}
df['iso_a3'] = df['iso2'].map(iso_mapping)

europe_data_map = world.merge(df, left_on='ADM0_A3', right_on='iso_a3', how='right')
europe_background = world[(world['CONTINENT'] == 'Europe') & (~world['ADM0_A3'].isin(df['iso_a3']))]

# --- 4. Design și Vizualizare ---
fig, ax = plt.subplots(1, 1, figsize=(14, 10), facecolor='white')

# Țările non-UE
europe_background.plot(ax=ax, color='#f0f0f0', edgecolor='#dcdcdc', linewidth=0.5)

# Plot principal (AM SCOS LEGENDA AICI)
europe_data_map.plot(
    column='vat_gap', 
    ax=ax, 
    cmap='YlGnBu_r',
    scheme='FisherJenks',
    k=5,
    edgecolor='#444444',
    linewidth=0.4,
    legend=False  # Legenda a fost eliminată
)

# --- 5. Adăugare Etichete (Inclusiv Malta Manual) ---
path_effect = [pe.withStroke(linewidth=3, foreground="white", alpha=0.8)]

for idx, row in europe_data_map.iterrows():
    if row.geometry is None: 
        continue
    
    # Logică coordonate: Manual pentru Malta, reprezentativ pentru restul
    if row['iso2'] == 'MT':
        # Acestea sunt coordonatele X și Y în proiecția 3035
        x, y = 5120000, 1580000 
    else:
        centroid = row.geometry.representative_point()
        x, y = centroid.x, centroid.y
        
        # Ajustări pentru vizibilitate (în metri)
        if row['iso2'] == 'FR': x -= 100000; y += 50000
        if row['iso2'] == 'IT': x += 50000
        if row['iso2'] == 'EL': y -= 100000

    # O singură comandă de scriere pentru toate țările
    ax.annotate(
        text=f"{row['iso2']}\n{row['vat_gap']:.1f}%", 
        xy=(x, y),
        ha='center', va='center',
        fontsize=8.5, 
        color='#1a1a1a', 
        weight='bold',
        path_effects=path_effect
    )

# --- 6. Finalizare Estetică ---
ax.set_xlim(2500000, 6500000)
ax.set_ylim(1400000, 5400000)
ax.set_axis_off()

plt.title("VAT Gap în Uniunea Europeană (2023)", fontsize=18, weight='bold', pad=10)
plt.tight_layout()
plt.show()
