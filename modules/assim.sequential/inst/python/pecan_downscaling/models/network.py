"""Residual proxy network with stable parameter names for checkpoint loading."""

import torch
from torch import nn


class ResidualBlock(nn.Module):
    def __init__(self, width: int) -> None:
        super().__init__()
        self.norm = nn.LayerNorm(width)
        self.first = nn.Linear(width, width * 2)
        self.second = nn.Linear(width * 2, width)

    def forward(self, values: torch.Tensor) -> torch.Tensor:
        residual = self.second(torch.nn.functional.silu(self.first(self.norm(values))))
        return values + residual


class Backbone(nn.Module):
    def __init__(
        self,
        numeric_count: int,
        width: int,
        blocks: int,
        use_year_embedding: bool = True,
        lc_categories: int = 12,
    ) -> None:
        super().__init__()
        self.use_year_embedding = use_year_embedding
        self.lc_embedding = nn.Embedding(lc_categories, 4)
        self.year_embedding = nn.Embedding(13, 4) if use_year_embedding else None
        categorical_count = 4 + (4 if use_year_embedding else 0)
        self.input_feature_count = numeric_count + categorical_count
        self.input = nn.Linear(self.input_feature_count, width)
        self.blocks = nn.ModuleList([ResidualBlock(width) for _ in range(blocks)])
        self.output_norm = nn.LayerNorm(width)

    def forward(
        self, numeric: torch.Tensor, lc: torch.Tensor, year: torch.Tensor
    ) -> torch.Tensor:
        categorical = [self.lc_embedding(lc)]
        if self.year_embedding is not None:
            categorical.append(self.year_embedding(year))
        values = torch.cat((numeric, *categorical), dim=1)
        values = torch.nn.functional.silu(self.input(values))
        for block in self.blocks:
            values = block(values)
        return self.output_norm(values)


class ProxyNetwork(nn.Module):
    def __init__(
        self,
        numeric_count: int,
        width: int,
        blocks: int,
        sharing: str,
        use_year_embedding: bool = True,
        lc_categories: int = 12,
    ) -> None:
        super().__init__()
        self.sharing = sharing
        self.use_year_embedding = use_year_embedding
        if sharing == "shared":
            self.backbone = Backbone(
                numeric_count, width, blocks, use_year_embedding, lc_categories
            )
            self.input_feature_count = self.backbone.input_feature_count
            self.heads = nn.ModuleList(
                [
                    nn.Sequential(
                        nn.Linear(width, 128),
                        nn.SiLU(),
                        nn.Linear(128, 2),
                    )
                    for _ in range(2)
                ]
            )
        elif sharing == "separate":
            self.backbones = nn.ModuleList(
                [
                    Backbone(
                        numeric_count, width, blocks, use_year_embedding, lc_categories
                    )
                    for _ in range(2)
                ]
            )
            self.input_feature_count = self.backbones[0].input_feature_count
            self.heads = nn.ModuleList(
                [
                    nn.Sequential(
                        nn.Linear(width, 128),
                        nn.SiLU(),
                        nn.Linear(128, 2),
                    )
                    for _ in range(2)
                ]
            )
        else:
            raise ValueError(f"Unknown sharing mode: {sharing}")

    def forward(
        self, numeric: torch.Tensor, lc: torch.Tensor, year: torch.Tensor
    ) -> torch.Tensor:
        outputs: list[torch.Tensor] = []
        if self.sharing == "shared":
            hidden = self.backbone(numeric, lc, year)
            outputs = [head(hidden) for head in self.heads]
        else:
            outputs = [
                head(backbone(numeric, lc, year))
                for backbone, head in zip(self.backbones, self.heads)
            ]
        return torch.stack(outputs, dim=1)
