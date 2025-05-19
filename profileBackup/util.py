def humanReadableSize(sizeBytes: int) -> str:
    """Convert a file size in bytes to a human-readable format (KB, MB, GB, etc.)."""
    if sizeBytes == 0:
        return "0 B"

    units = ["B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"]
    unitIdx = 0
    size = float(sizeBytes)

    while size >= 1024 and unitIdx < len(units) - 1:
        size /= 1024
        unitIdx += 1

    return f"{size:.2f} {units[unitIdx]}"  # 2 decimal places (e.g., "3.14 MB")
