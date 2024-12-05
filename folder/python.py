import matplotlib.pyplot as plt
import numpy as np

# Define the x and z coordinates for the folded surface (z = x^2)
x = np.linspace(-2, 2, 400)
z = x**2

# Create the plot
plt.figure(figsize=(8, 6))

# Plot the folded surface
plt.plot(x, z, label=r"$z = x^2$ (Ambient Space $X$)", color='blue')

# Add the x-axis as the observable space (M)
plt.axhline(0, color='gray', linestyle='--', label="Observable Space ($M$)")

# Highlight the singularity (fold point at the origin)
plt.scatter(0, 0, color='red', label="Singular Point ($p$)", zorder=5)

# Annotate the plot
plt.text(-1, 1.5, "Ambient Space ($X$)", fontsize=10, color='blue', ha='center')
plt.text(1.5, 0.2, "Observable Space ($M$)", fontsize=10, color='gray')
plt.text(0.1, 0.2, "$p$", fontsize=12, color='red')

# Add a grid, legend, and titles
plt.grid(alpha=0.4)
plt.title("Forcing Index Example: Folded Surface Projection", fontsize=14, fontweight="bold")
plt.xlabel("x (Observable Dimension)")
plt.ylabel("z (Hidden Dimension)")
plt.legend(fontsize=10)

plt.show()
# Show the plot